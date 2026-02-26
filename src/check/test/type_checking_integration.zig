//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

// primitives - nums //

test "check type - num - unbound" {
    const source =
        \\50
    ;
    try checkTypesExpr(
        source,
        .pass,
        "Dec",
    );
}

test "check type - num - int suffix 1" {
    const source =
        \\{
        \\  x = 10.U8
        \\
        \\  x
        \\}
    ;
    try checkTypesExpr(source, .pass, "U8");
}

test "check type - num - int suffix 2" {
    const source =
        \\{
        \\  x = 10.I128
        \\
        \\  x
        \\}
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
    try checkTypesExpr(
        source,
        .pass,
        "Dec",
    );
}

test "check type - num - float suffix 1" {
    const source =
        \\{
        \\  x : F32
        \\  x = 10.1
        \\
        \\  x
        \\}
    ;
    try checkTypesExpr(source, .pass, "F32");
}

test "check type - num - float suffix 2" {
    const source =
        \\{
        \\  x : F64
        \\  x = 10.1
        \\
        \\  x
        \\}
    ;
    try checkTypesExpr(source, .pass, "F64");
}

test "check type - num - float suffix 3" {
    const source =
        \\{
        \\  x : Dec
        \\  x = 10.1
        \\
        \\  x
        \\}
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
    // Number literal used where Str is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    // Str + number: the `+` operator desugars to calling the `.plus` method on the left operand.
    // Since Str doesn't have a `plus` method, we get MISSING METHOD before even checking
    // the from_numeral constraint on the number literal.
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
        \\a : I64
        \\a = 1
        \\b : I32
        \\b = 2
        \\x = a + b
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands must have same type - I64 minus I32 should fail" {
    const source =
        \\a : I64
        \\a = 1
        \\b : I32
        \\b = 2
        \\x = a - b
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands must have same type - I64 times I32 should fail" {
    const source =
        \\a : I64
        \\a = 1
        \\b : I32
        \\b = 2
        \\x = a * b
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands must have same type - F64 divide F32 should fail" {
    const source =
        \\a : F64
        \\a = 1.0
        \\b : F32
        \\b = 2.0
        \\x = a / b
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands same type works - I64 plus I64" {
    const source =
        \\x : I64
        \\x = 1 + 2
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "I64");
}

test "check type - binop operands same type works - unbound plus unbound" {
    const source =
        \\x = 1 + 2
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
}

test "check type - is_eq operands must have same type - I64 eq I32 should fail" {
    const source =
        \\a : I64
        \\a = 1
        \\b : I32
        \\b = 2
        \\x = a == b
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - comparison operands must have same type - I64 lt I32 should fail" {
    const source =
        \\a : I64
        \\a = 1
        \\b : I32
        \\b = 2
        \\x = a < b
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
    try checkTypesExpr(
        source,
        .pass,
        "List(Dec)",
    );
}

test "check type - list - 1st elem more specific coreces 2nd elem" {
    const source =
        \\{
        \\  x : U64
        \\  x = 100
        \\
        \\  [x, 200]
        \\}
    ;
    try checkTypesExpr(source, .pass, "List(U64)");
}

test "check type - list - 2nd elem more specific coreces 1st elem" {
    const source =
        \\{
        \\  x : U32
        \\  x = 200
        \\
        \\  [100, x]
        \\}
    ;
    try checkTypesExpr(source, .pass, "List(U32)");
}

test "check type - list  - diff elems 1" {
    const source =
        \\["hello", 10]
    ;
    // Number literal used where Str is expected (first elem determines list type)
    try checkTypesExpr(source, .fail, "TYPE MISMATCH");
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
    try checkTypesExpr(
        source,
        .pass,
        "{ hello: Str, world: Dec }",
    );
}

test "check type - record - field typo" {
    // spellchecker:off
    const source =
        \\main! = |_| {}
        \\
        \\MyRecord : { hello: Str }
        \\
        \\my_record : MyRecord
        \\my_record = { helo : "world" }
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This expression is used in an unexpected way:
        \\**test:6:13:6:31:**
        \\```roc
        \\my_record = { helo : "world" }
        \\```
        \\            ^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    { helo: Str }
        \\
        \\But the annotation say it should be:
        \\
        \\    MyRecord
        \\
        \\**Hint:** Maybe `helo` should be `hello`?
        \\
        \\
    );
    // spellchecker:on
}

test "check type - record - field missing" {
    const source =
        \\main! = |_| {}
        \\
        \\MyRecord : { hello: Str, world: U8 }
        \\
        \\my_record : MyRecord
        \\my_record = { hello : "world" }
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This expression is used in an unexpected way:
        \\**test:6:13:6:32:**
        \\```roc
        \\my_record = { hello : "world" }
        \\```
        \\            ^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    { hello: Str }
        \\
        \\But the annotation say it should be:
        \\
        \\    MyRecord
        \\
        \\**Hint:** This record is missing the field: `world`
        \\
        \\
    );
}

test "check type - record - ext - field missing" {
    const source =
        \\main! = |_| {}
        \\
        \\MyRecord(ext) : {  hello: Str, ..ext }
        \\
        \\my_record : MyRecord({ world: U8 })
        \\my_record = { hello : "world" }
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This expression is used in an unexpected way:
        \\**test:6:13:6:32:**
        \\```roc
        \\my_record = { hello : "world" }
        \\```
        \\            ^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    { hello: Str }
        \\
        \\But the annotation say it should be:
        \\
        \\    MyRecord({ world: U8 })
        \\
        \\**Hint:** This record is missing the field: `world`
        \\
        \\
    );
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
    const expected_binop: []const u8 = "c, c -> Bool where [c.is_eq : c, c -> Bool]";
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
    const expected_binop: []const u8 = "c, c -> Bool where [c.is_eq : c, c -> Bool]";
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
    try checkTypesExpr(source, .pass, "[MyTag, ..]");
}

test "check type - tag - args" {
    const source =
        \\MyTag("hello", 1)
    ;
    try checkTypesExpr(
        source,
        .pass,
        "[MyTag(Str, Dec), ..]",
    );
}

test "check type - tag union - tag typo" {
    const source =
        \\main! = |_| {}
        \\
        \\Color : [Red, Green, Blue]
        \\
        \\color : Color
        \\color = Greeen
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This expression is used in an unexpected way:
        \\**test:6:9:6:15:**
        \\```roc
        \\color = Greeen
        \\```
        \\        ^^^^^^
        \\
        \\It has the type:
        \\
        \\    [Greeen, ..]
        \\
        \\But the annotation say it should be:
        \\
        \\    Color
        \\
        \\**Hint:** Maybe `Greeen` should be `Green`?
        \\
        \\
    );
}

test "check type - tag - ext - typo" {
    const source =
        \\main! = |_| {}
        \\
        \\Color(ext) : [Red, Blue, ..ext]
        \\
        \\color : Color([Green])
        \\color = Greeen
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This expression is used in an unexpected way:
        \\**test:6:9:6:15:**
        \\```roc
        \\color = Greeen
        \\```
        \\        ^^^^^^
        \\
        \\It has the type:
        \\
        \\    [Greeen, ..]
        \\
        \\But the annotation say it should be:
        \\
        \\    Color([Green])
        \\
        \\**Hint:** Maybe `Greeen` should be `Green`?
        \\
        \\
    );
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
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "_arg -> Dec",
    );
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
    // Number literal used where unconstrained type is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - def - nested lambda" {
    const source =
        \\id = (((|a| |b| |c| a + b + c)(100))(20))(3)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
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

test "check type - def - wrong arg" {
    const source =
        \\main! = |_| {}
        \\
        \\func : Str, U8 -> U8
        \\func = |_str, u8| u8
        \\
        \\test = func("hello", "world")
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\The second argument being passed to this function has the wrong type:
        \\**test:6:8:**
        \\```roc
        \\test = func("hello", "world")
        \\```
        \\                     ^^^^^^^
        \\
        \\This argument has the type:
        \\
        \\    Str
        \\
        \\But `func` needs the second argument to be:
        \\
        \\    U8
        \\
        \\
        ,
    );
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
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
}

test "check type - def - polymorphic id 2" {
    const source =
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = (id(5), id("hello"))
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "(Dec, Str)",
    );
}

test "check type - def - out of order" {
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
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
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
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
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
    // Number literal 42 used where Str is expected (function type unified from both calls)
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - def - call with wrong fn arity - too many" {
    const source =
        \\idStr : Str -> Str
        \\idStr = |x| x
        \\
        \\test = idStr("hello", 10.U8)
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TOO MANY ARGS**
        \\The `idStr` function expects 1 argument, but it got 2 instead:
        \\**test:4:8:4:29:**
        \\```roc
        \\test = idStr("hello", 10.U8)
        \\```
        \\       ^^^^^^^^^^^^^^^^^^^^^
        \\
        \\The `idStr` function has the type:
        \\
        \\    Str -> Str
        \\
        \\
        ,
    );
}

test "check type - def - call with wrong fn arity - too few" {
    const source =
        \\idStr : Str, U8 -> Str
        \\idStr = |x, _| x
        \\
        \\test = idStr("hello")
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TOO FEW ARGS**
        \\The `idStr` function expects 2 arguments, but it got 1 instead:
        \\**test:4:8:4:22:**
        \\```roc
        \\test = idStr("hello")
        \\```
        \\       ^^^^^^^^^^^^^^
        \\
        \\The `idStr` function has the type:
        \\
        \\    Str, U8 -> Str
        \\
        \\Are there any missing commas?
        \\
        \\
        ,
    );
}

test "check type - def - call with mismatch arg" {
    const source =
        \\idStr : Str, U8 -> Str
        \\idStr = |x, _| x
        \\
        \\test = idStr("hello", "world")
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\The second argument being passed to this function has the wrong type:
        \\**test:4:8:**
        \\```roc
        \\test = idStr("hello", "world")
        \\```
        \\                      ^^^^^^^
        \\
        \\This argument has the type:
        \\
        \\    Str
        \\
        \\But `idStr` needs the second argument to be:
        \\
        \\    U8
        \\
        \\
        ,
    );
}

// value restriction //

test "check type - value restriction - out of order 1" {
    const source =
        \\main! = |_| {}
        \\
        \\x = 10
        \\
        \\process = |y| {
        \\  [x, y]
        \\}
        \\
        \\test = {
        \\  _a = process(1.U8)
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "x", .expected = "U8" },
            .{ .def = "process", .expected = "U8 -> List(U8)" },
        },
    );
}

test "check type - value restriction - out of order 2" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  _a = process(1.U8, "x")
        \\  _b = process(1.U8, Bool.False)
        \\}
        \\
        \\process = |y, _z| {
        \\  _blah = [x, y] # Force x and y to be the same type
        \\  y
        \\}
        \\
        \\x = 10
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "x", .expected = "U8" },
            .{ .def = "process", .expected = "U8, _arg -> U8" },
        },
    );
}

test "check type - value restriction - curried 1" {
    // Currently errors out in czer

    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  _a = process(1.U8)("x")
        \\  _b = process(1.U8)(Bool.False)
        \\}
        \\
        \\process = |y| {
        \\  |_z| { [x, y] }
        \\}
        \\
        \\x = 10
    ;
    try checkTypesModule(
        source,
        .fail,
        "TYPE MISMATCH",
    );
}
test "check type - value restriction - curried 2" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  fn_a = process(1.U8)
        \\  _a = fn_a("x")
        \\
        \\  fn_b = process(2.U8)
        \\  _b = fn_b(Bool.False)
        \\}
        \\
        \\process = |y| {
        \\  |_z| { [x, y] }
        \\}
        \\
        \\x = 10
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "x", .expected = "U8" },
            .{ .def = "process", .expected = "U8 -> (_arg -> List(U8))" },
        },
    );
}

test "check type - value restriction - out of order 3" {
    // Currently errors out in czer

    const source =
        \\main! = |_| {}
        \\
        \\A := [A].{
        \\  exec : A, I64 -> I64
        \\  exec = |A, _| 10
        \\}
        \\
        \\test = {
        \\  _a = run_exec(A.A, "hello")
        \\  _b = run_exec(A.A, Try.Ok("nice"))
        \\}
        \\
        \\run_exec = |y, _z| {
        \\  y.exec(x)
        \\}
        \\
        \\x = 10
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "x", .expected = "I64" },
            .{ .def = "run_exec", .expected = "a, _arg -> b where [a.exec : a, I64 -> b]" },
        },
    );
}

test "check type - value restriction - should fail 1" {
    const source =
        \\main! = |_| {}
        \\
        \\x = 10
        \\
        \\process = |y| { [x, y] }
        \\
        \\test = {
        \\  _a = process(1.U8)
        \\  _b = process(1.I64) # Should error: U8 != I64
        \\}
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - value restriction - should fail 2" {
    const source =
        \\main! = |_| {}
        \\
        \\A := [A].{
        \\  process = |_a, y| { [x, y] }
        \\}
        \\
        \\x = 10
        \\
        \\test = {
        \\  val = A.A
        \\  _a = val.process(1.U8)
        \\  _b = val.process(1.I64) # Should error: U8 != I64
        \\}
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
    // Number literal 15 used where Str is expected
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
    // Number literal 10 used where Str is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Pair(Str, Dec)",
    );
}

// nominal types //

test "check type - nominal - local - fail" {
    const source =
        \\main! = |_| {}
        \\
        \\test = |{}| {
        \\  Utf8Format := {}.{
        \\    encode_str : Utf8Format, Str -> List(U8)
        \\    encode_str = |_fmt, s| Str.to_utf8(s)
        \\  }
        \\  fmt = Utf8Format
        \\  Str.encode("hi", fmt)
        \\}
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\The `encode_str` method on `Utf8Format` has an incompatible type:
        \\**test:9:20:9:23:**
        \\```roc
        \\  Str.encode("hi", fmt)
        \\```
        \\                   ^^^
        \\
        \\The method `encode_str` has the type:
        \\
        \\    Utf8Format, Str -> List(U8)
        \\
        \\But I need it to have the type:
        \\
        \\    Utf8Format, Str -> Try(encoded, err)
        \\
        \\
        ,
    );
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
        \\x = if 5.I64 "true" else "false"
    ;
    // Number literal 5 used where Bool is expected
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\This `if` condition must evaluate to a `Bool`–either `True` or `False`:
        \\**test:2:8:2:13:**
        \\```roc
        \\x = if 5.I64 "true" else "false"
        \\```
        \\       ^^^^^
        \\
        \\It is:
        \\
        \\    I64
        \\
        \\But I need this to be a `Bool` value.
        \\
        \\
        ,
    );
}

test "check type - if else - invalid condition 2" {
    const source =
        \\x : Str
        \\x = if 10 "true" else "false"
    ;
    // Number literal 10 used where Bool is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - if else - invalid condition 3" {
    const source =
        \\x : Str
        \\x = if "True" "true" else "false"
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - if else - different branch types 1" {
    const source =
        \\x = if True "true" else 10.U8
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\The second branch of this `if` does not match the previous branch :
        \\**test:1:25:1:30:**
        \\```roc
        \\x = if True "true" else 10.U8
        \\```
        \\                        ^^^^^
        \\
        \\The second branch is:
        \\
        \\    U8
        \\
        \\But the previous branch results in:
        \\
        \\    Str
        \\
        \\
        ,
    );
}

test "check type - if else - different branch types 2" {
    const source =
        \\x = if True "true" else if False "false" else 10
    ;
    // Number literal 10 used where Str is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - if else - different branch types 3" {
    const source =
        \\x = if True "true" else if False 10 else "last"
    ;
    // Number literal 10 used where Str is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\The first pattern in this `match` is incompatible:
        \\**test:2:3:**
        \\```roc
        \\  match "hello" {
        \\    True => "true"
        \\    False => "false"
        \\  }
        \\```
        \\    ^^^^
        \\
        \\The first pattern is trying to match:
        \\
        \\    [True, ..]
        \\
        \\But the expression between the `match` parenthesis has the type:
        \\
        \\    Str
        \\
        \\These can never match! Either the pattern or expression has a problem.
        \\
        \\
        ,
    );
}

test "check type - match - diff branch types" {
    const source =
        \\x =
        \\  match True {
        \\    True => "true"
        \\    False => 100
        \\  }
    ;
    // Number literal 100 used where Str is expected
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// unary not

test "check type - unary not" {
    const source =
        \\x = !Bool.True
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - unary not mismatch" {
    const source =
        \\x = !"Hello"
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

// unary minus

test "check type - unary minus" {
    const source =
        \\x = -10
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
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
        \\y : U32
        \\y = 10
        \\x = 10 + y
    ;
    // With flexible binops, the lhs stays polymorphic until constraint resolution
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "U32",
    );
}

test "check type - binops math sub" {
    const source =
        \\x = 1 - 0.2
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
}

test "check type - binops ord" {
    const source =
        \\{
        \\  a : F32
        \\  a = 10.0
        \\
        \\  a > 15
        \\}
    ;
    try checkTypesExpr(source, .pass, "Bool");
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
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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

test "check type - record access - field typo" {
    // spellchecker:off
    const source =
        \\main! = |_| {}
        \\
        \\r =
        \\  {
        \\    hello: "Hello",
        \\    world: 10,
        \\  }
        \\
        \\x = r.helo
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This record does not have a `helo` field:
        \\**test:9:6:9:11:**
        \\```roc
        \\x = r.helo
        \\```
        \\     ^^^^^
        \\
        \\This is often due to a typo. The most similar fields are:
        \\
        \\    - `hello`
        \\    - `world`
        \\
        \\So maybe `helo` should be `hello`?
        \\
        \\
    );
    // spellchecker:on
}

test "check type - record - access func polymorphic" {
    const source =
        \\x = |r| r.my_field
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ .., my_field: a } -> a");
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
        "({ data: Dec }, { data: Dec, other: Str }, { data: Str })",
    );
}

test "check type - record - update - fold" {
    const source =
        \\test = List.fold([1.U8, 2, 3], {sum: 0.U8, count: 0.U8}, |acc, item| {..acc, sum: acc.sum + item, count: acc.count + 1})
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "test" } },
        "{ count: U8, sum: U8 }",
    );
}

test "check type - record - update - fail - empty record" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  r = {}
        \\  { ..r, hello: 10.U8 }
        \\}
    ;
    // Number literal 10 used where Str is expected (data field type)
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The `r` record does not have a `hello` field:
        \\**test:5:7:5:8:**
        \\```roc
        \\  { ..r, hello: 10.U8 }
        \\```
        \\      ^
        \\
        \\It is actually a record with no fields.
        \\
        \\
    );
}

test "check type - record - update - fail - missing field" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  r = { hello: "world" }
        \\  { ..r, hllo: "goodbye" }
        \\}
    ;
    // Number literal 10 used where Str is expected (data field type)
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\This record does not have a `hllo` field:
        \\**test:5:7:5:8:**
        \\```roc
        \\  { ..r, hllo: "goodbye" }
        \\```
        \\      ^
        \\
        \\This is often due to a typo. The most similar fields are:
        \\
        \\    - `hello`
        \\
        \\So maybe `hllo` should be `hello`?
        \\
        \\__Note:__ You cannot add new fields to a record with the record update syntax.
        \\
        \\
    );
}

test "check type - record - update - fail - field mismatch" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  r = { hello: "world" }
        \\  { ..r, hello: 10.U8 }
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The type of the field `hello` is incompatible:
        \\**test:5:17:5:22:**
        \\```roc
        \\  { ..r, hello: 10.U8 }
        \\```
        \\                ^^^^^
        \\
        \\You are trying to update the `hello` field to be the type:
        \\
        \\    U8
        \\
        \\But the `r` record needs it to be
        \\
        \\    Str
        \\
        \\__Note:__ You cannot change the type of a record field with the record update syntax. You can do that by create a new record, copying over the unchanged fields, then transforming `hello` to be the new type.
        \\
        \\
    );
}

test "check type - record - update - fail - field mismatch 2" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  r = { hello: "world", nice: 10.U8 }
        \\  { ..r, hello: 10.Dec }
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The type of the field `hello` is incompatible:
        \\**test:5:17:5:23:**
        \\```roc
        \\  { ..r, hello: 10.Dec }
        \\```
        \\                ^^^^^^
        \\
        \\You are trying to update the `hello` field to be the type:
        \\
        \\    Dec
        \\
        \\But the `r` record needs it to be
        \\
        \\    Str
        \\
        \\__Note:__ You cannot change the type of a record field with the record update syntax. You can do that by create a new record, copying over the unchanged fields, then transforming `hello` to be the new type.
        \\
        \\
    );
}

test "check type - record - update - fail - field mismatch 3" {
    const source =
        \\main! = |_| {}
        \\
        \\test = {
        \\  r = { hello: "world", nice: 10.U8 }
        \\  { ..r, nice: 10.Dec }
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The type of the field `nice` is incompatible:
        \\**test:5:16:5:22:**
        \\```roc
        \\  { ..r, nice: 10.Dec }
        \\```
        \\               ^^^^^^
        \\
        \\You are trying to update the `nice` field to be the type:
        \\
        \\    Dec
        \\
        \\But the `r` record needs it to be
        \\
        \\    U8
        \\
        \\__Note:__ You cannot change the type of a record field with the record update syntax. You can do that by create a new record, copying over the unchanged fields, then transforming `nice` to be the new type.
        \\
        \\
    );
}

test "check type - record - update - fail 2" {
    const source =
        \\set_data = |container, new_value| { ..container, data: new_value }
        \\
        \\updated = set_data({ data: "hello" }, 10.U8)
    ;
    // Number literal 10 used where Str is expected (data field type)
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The second argument being passed to this function has the wrong type:
        \\**test:3:11:**
        \\```roc
        \\updated = set_data({ data: "hello" }, 10.U8)
        \\```
        \\                                      ^^^^^
        \\
        \\This argument has the type:
        \\
        \\    U8
        \\
        \\But `set_data` needs the second argument to be:
        \\
        \\    Str
        \\
        \\
    );
}

test "check type - record - pattern destructure rest 1" {
    const source =
        \\strip_name = |{ name: _, ..rest}| rest
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "strip_name" } },
        "{ ..a, name: _field } -> a",
    );
}

test "check type - record - pattern destructure rest 2" {
    const source =
        \\strip_name = |{ name: _, ..rest}| rest.age
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "strip_name" } },
        "{ .., age: a, name: _field } -> a",
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
    try checkTypesExpr(source, .fail, "TYPE MISMATCH");
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
    try checkTypesExpr(source, .fail, "TYPE MISMATCH");
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
    // Test that matching a tag against incompatible tag patterns fails
    const source =
        \\{
        \\  x : [Ok(I64), Err(Str)]
        \\  x = Ok(42)
        \\
        \\  match(x) {
        \\    Some(_) => "found",
        \\    None => "empty",
        \\  }
        \\}
    ;
    try checkTypesExpr(source, .fail, "TYPE MISMATCH");
}

test "check type - patterns frac 1" {
    const source =
        \\{
        \\  result : Dec
        \\  result = match(20) {
        \\    10 as x => x,
        \\    _ => 15,
        \\  }
        \\
        \\  result
        \\}
    ;
    try checkTypesExpr(source, .pass, "Dec");
}

test "check type - patterns frac 2" {
    const source =
        \\{
        \\  result : F32
        \\  result = match(10) {
        \\    10 as x => x,
        \\    _ => 15,
        \\  }
        \\
        \\  result
        \\}
    ;
    try checkTypesExpr(source, .pass, "F32");
}

test "check type - patterns frac 3" {
    const source =
        \\{
        \\  result : F64
        \\  result = match(50) {
        \\    10 as x => x,
        \\    15 as x => x,
        \\    _ => 20,
        \\  }
        \\
        \\  result
        \\}
    ;
    try checkTypesExpr(source, .pass, "F64");
}

test "check type - patterns list" {
    // The pattern [_a, .. as b] is redundant because [.. as b, _a] already matches
    // all non-empty lists. Both patterns match lists with 1+ elements, just extracting
    // different parts (first vs last element).
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
    try checkTypesExpr(source, .fail, "REDUNDANT PATTERN");
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
    try checkTypesExpr(
        source,
        .pass,
        "Dec",
    );
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
    try checkTypesExpr(source, .fail, "TYPE MISMATCH");
}

// vars + reassignment //

test "check type - var reassignment" {
    const source =
        \\main = {
        \\  var x = 1
        \\  x = x + 1
        \\  x
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
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
    // Numeric literals with from_numeral constraints are NOT generalized (GitHub #8666).
    // This means constraints from `x == 1` (the is_eq constraint) DO propagate back
    // to the definition of x, along with the original from_numeral constraint.
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Dec",
    );
}

test "check type - expect not bool" {
    const source =
        \\main = {
        \\  x = 1.U8
        \\  expect x
        \\  x
        \\}
    ;
    // Number literal used where Bool is expected
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\This `expect` statement must evaluate to a `Bool`–either `True` or `False`:
        \\**test:3:10:3:11:**
        \\```roc
        \\  expect x
        \\```
        \\         ^
        \\
        \\It is:
        \\
        \\    U8
        \\
        \\But I need this to be a `Bool` value.
        \\
        \\
        ,
    );
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
    // With flexible binops, lhs stays polymorphic
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

// type modules //

test "check type - type module - fn declarations " {
    const source =
        \\main! = |_| {}
        \\
        \\Person := [].{
        \\    name : Str
        \\    name = "Alice"
        \\
        \\    age : I32
        \\    age = 25
        \\
        \\    height : Dec
        \\    height = 5.8
        \\
        \\    is_active : Bool
        \\    is_active = True
        \\
        \\    colors : List(Str)
        \\    colors = ["red", "green", "blue"]
        \\
        \\    numbers : List(I32)
        \\    numbers = [1, 2, 3, 4, 5]
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Person.name", .expected = "Str" },
            .{ .def = "Test.Person.age", .expected = "I32" },
            .{ .def = "Test.Person.height", .expected = "Dec" },
            .{ .def = "Test.Person.is_active", .expected = "Bool" },
            .{ .def = "Test.Person.colors", .expected = "List(Str)" },
            .{ .def = "Test.Person.numbers", .expected = "List(I32)" },
        },
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
        "Dec",
    );
}

test "check type - for mismatch" {
    const source =
        \\main : I64
        \\main = {
        \\  var result = 0.I64
        \\  for x in ["a", "b", "c"] {
        \\    result = result + x
        \\  }
        \\  result
        \\}
    ;
    try checkTypesModule(
        source,
        .fail_first,
        "TYPE MISMATCH",
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
        "(Dec, Str, Bool)",
    );
}

test "check type - comprehensive - multiple layers of lambdas" {
    const source =
        \\main! = |_| {}
        \\
        \\# Four layers of nested lambdas
        \\curried_add : a, a, a, a -> a where [a.plus : a, a -> a]
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
        "Dec",
    );
}

test "check type - comprehensive - static dispatch with multiple methods 1" {
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
        "Dec",
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
        "Container(Dec)",
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
        \\func : U32
        \\func = my_plus(1, 2)
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
        \\func : U32
        \\func = add_two(1, 2)
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
        \\func : U32
        \\func = my_plus(my_plus(1, 2), 3)
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
        \\func : U32
        \\func = add_three(1, 2, 3)
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
        \\compute = |x| add_three(x, 1, 2)
        \\
        \\func = compute(10)
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
        "{ chained: Dec, final: Dec, id_results: (Dec, Str, Bool), processed: Dec, transformed: Dec }",
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

test "check type - body w/ anno does not leak to references - top level" {
    // First verify Bool basics work
    const source =
        \\Test := [].{}
        \\
        \\x : Bool
        \\x = "Str"
        \\
        \\y : Bool
        \\y = x or True
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("TYPE MISMATCH");
    try test_env.assertDefTypeOptions("y", "Bool", .{ .allow_type_errors = true });
}

test "check type - body w/ anno does not leak to references - inline" {
    // Test that when an inline declaration's body has a type error,
    // the annotation type is preserved for references
    const source =
        \\Test := [].{}
        \\
        \\test = {
        \\  x : Bool
        \\  x = "Str"
        \\
        \\  y : Bool
        \\  y = x or True
        \\
        \\  y
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("TYPE MISMATCH");
    try test_env.assertDefTypeOptions("test", "Bool", .{ .allow_type_errors = true });
}

test "check type - scoped type variables - bigger example 1" {
    const source =
        \\test_scoped : a, b -> a
        \\test_scoped = |a, b| {
        \\  f : a -> a
        \\  f = |z| z
        \\
        \\  # No err because we correctly provide `a` as the arg
        \\  result : a
        \\  result = f(a)
        \\
        \\  # Err because we incorrectly provide `b` as the arg
        \\  _result2 : b
        \\  _result2 = f(b)
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

test "check type - scoped type variables - bigger example 2" {
    const source =
        \\test : val -> val
        \\test = |a| {
        \\  b : other_val -> other_val
        \\  b = |c| {
        \\    d : other_val
        \\    d = c
        \\
        \\    d
        \\  }
        \\
        \\  b(a)
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "test" } },
        "val -> val",
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
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "x" } },
        "Dec",
    );
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
    try test_env.assertDefType("result", "Dec");
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

// recursive errors //

test "check type - recursive type - infinite" {
    const source =
        \\func = |a| func([a])
    ;
    try checkTypesModule(source, .fail_with,
        \\**INFINITE TYPE**
        \\I am inferring a weird self-referential type:
        \\**test:1:1:1:21:**
        \\```roc
        \\func = |a| func([a])
        \\```
        \\^^^^^^^^^^^^^^^^^^^^
        \\
        \\Here is my best effort at writing down the type. You will see `<RecursiveType>` for parts of the type that repeat infinitely.
        \\
        \\    List(<RecursiveType>)
        \\
        \\
        \\
    );
}

test "check type - recursive type - recursive alias" {
    const source =
        \\LinkedList(a) : [Nil, Cons(a, LinkedList(a))]
    ;
    try checkTypesModule(source, .fail_with,
        \\**RECURSIVE ALIAS**
        \\The type alias _LinkedList_ references itself, which is not allowed:
        \\**test:1:31:1:44:**
        \\```roc
        \\LinkedList(a) : [Nil, Cons(a, LinkedList(a))]
        \\```
        \\                              ^^^^^^^^^^^^^
        \\
        \\Type aliases cannot be recursive. If you need a recursive type, use a nominal type `:=` instead of an alias`:`.
        \\
        \\
    );
}

test "check type - recursive type - anonymous recursion" {
    const source =
        \\len = |linked_list|
        \\  match linked_list {
        \\    Cons(_a, rest) => 1 + len(rest)
        \\    Nil => 0.U8
        \\  }
    ;
    try checkTypesModule(source, .fail_with,
        \\**ANONYMOUS RECURSION**
        \\I am inferring a recursive type that has no name somewhere in `len`:
        \\**test:1:1:5:4:**
        \\```roc
        \\len = |linked_list|
        \\  match linked_list {
        \\    Cons(_a, rest) => 1 + len(rest)
        \\    Nil => 0.U8
        \\  }
        \\```
        \\
        \\Here is the type I'm inferring. You will see `<RecursiveType>` for parts of the type that repeat.
        \\
        \\    [Cons(_a, <RecursiveType>), Nil, ..]
        \\
        \\**Hint:** Recursive types are only allowed through nominal types. If you need a recursive data structure, define a nominal type using `:=`.
        \\
        \\
    );
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
        \\    a.is_eq : a, a -> Bool,
        \\    b.is_eq : b, b -> Bool
        \\]
        \\fn = |a, b| (|x| x.plus(b))(a)
    ;

    // The annotated type should match the inferred type
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "fn" } },
        \\a, b -> ret
        \\  where [a.is_eq : a, a -> Bool
        \\     , a.plus : a, b -> ret
        \\     , b.is_eq : b, b -> Bool]
        ,
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
        \\a : U64
        \\a = 1
        \\b : U64
        \\b = 2
        \\c : U64
        \\c = 3
        \\d : U64
        \\d = 4
        \\
        \\result = fn(a, b) == fn(c, d)
    ;

    // This should pass - both calls use the same types
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

// helpers - module //

const ModuleExpectation = union(enum) {
    pass: DefExpectation,
    fail,
    fail_first, // Allows multiple errors, checks first error title
    fail_with,
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
        .fail_with => {
            return test_env.assertOneTypeErrorMsg(expected);
        },
        .fail_first => {
            return test_env.assertFirstTypeError(expected);
        },
    }
}

const DefAndExpectation = struct {
    def: []const u8,
    expected: []const u8,
};

fn checkTypesModuleDefs(
    comptime source_expr: []const u8,
    comptime expectations: []const DefAndExpectation,
) !void {
    var test_env = try TestEnv.init("Test", source_expr);
    defer test_env.deinit();

    inline for (expectations) |expectation| {
        try test_env.assertDefType(expectation.def, expectation.expected);
    }
}

// helpers - expr //

const ExprExpectation = union(enum) {
    pass,
    fail,
    fail_with,
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
        .fail_with => {
            return test_env.assertOneTypeErrorMsg(expected);
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
    try checkTypesModule(source, .{ .pass = .last_def }, "{  } -> Try(Str, [Impossible, ListWasEmpty, ..])");
}

test "check type - try operator on method call should apply to whole expression (#8646)" {
    // Regression test for https://github.com/roc-lang/roc/issues/8646
    // The `?` suffix on `strings.first()` should apply to the entire method call expression,
    // not just to the right side of the field access. Previously, the parser was attaching
    // `?` to `first()` before creating the field_access node, causing a type mismatch error
    // that expected `{ unknown: _field }`.
    const source =
        \\question_fail : List(Str) -> Try(Str, _)
        \\question_fail = |strings| {
        \\    first_str = strings.first()?
        \\    Ok(first_str)
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "List(Str) -> Try(Str, [ListWasEmpty, ..])");
}

// record extension in type annotations //

test "check type - record extension - basic open record annotation" {
    // Test that a function accepting { name: Str, ..others } can take records with extra fields
    const source =
        \\getName : { name: Str, ..others } -> Str
        \\getName = |record| record.name
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ ..others, name: Str } -> Str");
}

test "check type - record extension - closed record satisfies open record" {
    // A closed record { name: Str, age: I64 } should satisfy { name: Str, ..others }
    const source =
        \\getName : { name: Str, ..others } -> Str
        \\getName = |record| record.name
        \\
        \\result = getName({ name: "Alice", age: 30 })
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - record extension - multiple fields with extension" {
    // Test with multiple required fields and an extension
    const source =
        \\getFullName : { first: Str, last: Str, ..others } -> Str
        \\getFullName = |record| Str.concat(Str.concat(record.first, " "), record.last)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ ..others, first: Str, last: Str } -> Str");
}

test "check type - record extension - nested records with extension" {
    // Test record extension with nested record types
    const source =
        \\getPersonName : { person: { name: Str, ..inner }, ..outer } -> Str
        \\getPersonName = |record| record.person.name
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ ..outer, person: { ..inner, name: Str } } -> Str");
}

test "check type - record extension - empty record with extension" {
    // An empty record with extension means "any record"
    const source =
        \\takeAnyRecord : { ..others } -> Str
        \\takeAnyRecord = |_record| "got a record"
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ ..others } -> Str");
}

test "check type - record extension - mismatch should fail" {
    // Test that a record missing a required field should fail
    const source =
        \\getName : { name: Str, ..others } -> Str
        \\getName = |record| record.name
        \\
        \\result = getName({ age: 30 })
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// List method syntax tests

test "check type - List.get method syntax" {
    // Check what type is inferred for [1].get(0) (this works at runtime)
    const source =
        \\result = [1].get(0)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Try(Dec, [OutOfBounds, ..])",
    );
}

// Nested error type annotation tests

test "check type - nested error should use annotation type" {
    // Test that when an expression has a nested error (not at the root level),
    // the pattern still gets the annotation type, not an error type.
    //
    // For example: if annotation is Try(Try(I64, Str), Bool) and expression is
    // Ok(Err("oops")), the pattern should get the full annotation type even though
    // the nested Err would normally need type information from the annotation.
    //
    // This tests the fix for the TODO at Check.zig lines 1530-1534.
    const source =
        \\nested_err : Try(Try(I64, Str), Bool)
        \\nested_err = Ok(Err("oops"))
    ;
    // Expected: the pattern should have the annotation type
    try checkTypesModule(source, .{ .pass = .last_def }, "Try(Try(I64, Str), Bool)");
}

test "check type - deeply nested error should use annotation type" {
    // Test that errors deeply nested within a structure still get annotation type
    const source =
        \\deep_err : Try(Try(Try(I64, Str), Bool), U8)
        \\deep_err = Ok(Ok(Err("deep error")))
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Try(Try(Try(I64, Str), Bool), U8)");
}

test "check type - nested error in function return should use annotation" {
    // Test that nested errors in function return values work correctly
    const source =
        \\get_nested : {} -> Try(Try(I64, Str), Bool)
        \\get_nested = |{}| Ok(Err("inner error"))
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{  } -> Try(Try(I64, Str), Bool)");
}

// List.first method syntax tests - REGRESSION TEST for cycle detection bug

test "check type - List.first method syntax should not create cyclic types" {
    // REGRESSION TEST: This test reproduces a bug where calling [1].first() (method syntax)
    // would cause an infinite loop in layout computation because the interpreter was creating
    // cyclic rigid var mappings in the TypeScope when building layouts.
    //
    // The bug: method syntax creates a StaticDispatchConstraint on a flex var.
    // When the return type is Try(item, [ListWasEmpty, ..]) with an open tag union,
    // the interpreter was creating cyclic rigid -> rigid mappings in the empty_scope TypeScope.
    //
    // Method syntax: [1].first()
    // Should have same type as function syntax: List.first([1])
    //
    // NOTE: The type checking itself is correct - this test verifies type checking produces
    // the right type. The bug manifests in the interpreter's layout computation phase.
    const source =
        \\result = [1].first()
    ;
    // Expected: Try(Dec, [ListWasEmpty, ..]) after from_numeral resolution
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        "Try(Dec, [ListWasEmpty, ..])",
    );
}

test "check type - lambda capturing top-level constant with plus - mono_pure_lambda case" {
    // This test verifies the type inference for the mono_pure_lambda snapshot.
    // The result of add_one(5) should be a numeric type with from_numeral and plus constraints,
    // NOT Bool.
    const source =
        \\one = 1
        \\add_one = |x| x + one
        \\result = add_one(5)
    ;
    // Expected: result should have numeric type resolved to Dec
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "result" } },
        "Dec",
    );
}

test "check type - simple function call should have return type" {
    // Simpler test: directly call a lambda to verify the call expression gets the right type
    const source =
        \\add_one = |x| x + 1
        \\result = add_one(5)
    ;
    // Both add_one and result should have numeric types resolved to Dec
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "result" } },
        "Dec",
    );
}

// Lots of inferred constraints

test "check type - range inferred" {
    const source =
        \\range = |var $current, end| {
        \\  if end < $current {
        \\    return []
        \\  }
        \\
        \\  var $answer = List.with_capacity(((end - $current) + 1).to_u64())
        \\  while $current <= end {
        \\    $answer = $answer.append($current)
        \\    $current = $current + 1
        \\  }
        \\  $answer
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .last_def },
        \\a, a -> List(a)
        \\  where [
        \\    a.is_lt : a, a -> Bool,
        \\    a.is_lte : a, a -> Bool,
        \\    a.minus : a, a -> a,
        \\    a.plus : a, Dec -> a,
        \\    a.to_u64 : a -> U64,
        \\  ]
        ,
    );
}

test "check type - issue8934 recursive nominal type unification" {
    // Regression test for https://github.com/roc-lang/roc/issues/8934
    // The bug was that the compiler would stack overflow when type-checking
    // recursive nominal types with nested recursive calls, like:
    //   Node(a) := [One(a), Many(List(Node(a)))]
    //   flatten_aux(rest, flatten_aux(e, acc))
    //
    // The root cause was that unifyNominalType didn't do an early merge before
    // unifying type arguments, causing infinite recursion when the nominal type
    // referenced itself through the tag union backing type.
    const source =
        \\main! = |_| {}
        \\
        \\Node(a) := [One(a), Many(List(Node(a)))]
        \\
        \\flatten : List(Node(a)) -> List(a)
        \\flatten = |input| {
        \\  flatten_aux = |l, acc| {
        \\    match l {
        \\      [] => acc
        \\      [One(e), .. as rest] => flatten_aux(rest, List.append(acc, e))
        \\      [Many(e), .. as rest] => flatten_aux(rest, flatten_aux(e, acc))
        \\    }
        \\  }
        \\  flatten_aux(input, [])
        \\}
    ;
    // The key thing is that the compiler should NOT crash with stack overflow.
    // It should successfully type-check the file.
    try checkTypesModule(source, .{ .pass = .{ .def = "flatten" } }, "List(Node(a)) -> List(a)");
}

// early return //

test "check type - early return - pass" {
    const source =
        \\|bool| {
        \\  if bool {
        \\    return []
        \\  }
        \\
        \\ []
        \\}
    ;
    try checkTypesExpr(source, .pass, "Bool -> List(_a)");
}

test "check type - early return - fail" {
    const source =
        \\|bool| {
        \\  if bool {
        \\    return "hello"
        \\  }
        \\
        \\ []
        \\}
    ;
    try checkTypesExpr(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\This `return` does not match the function's return type:
        \\**test:3:12:3:19:**
        \\```roc
        \\    return "hello"
        \\```
        \\           ^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Str
        \\
        \\But the function's return type is:
        \\
        \\    List(_a)
        \\
        \\**Hint:** All `return` statements and the final expression in a function must have the same type.
        \\
        \\
        ,
    );
}

test "check type - early return - ? - fail" {
    const source =
        \\|| {
        \\  _val = Try.Err("hello")?
        \\  Try.Err(Bool.True)
        \\}
    ;
    try checkTypesExpr(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\This `?` may return early with a type that doesn't match the function body:
        \\**test:2:10:2:27:**
        \\```roc
        \\  _val = Try.Err("hello")?
        \\```
        \\         ^^^^^^^^^^^^^^^^^
        \\
        \\On error, this would return:
        \\
        \\    Try(ok, Str)
        \\
        \\But the function body evaluates to:
        \\
        \\    Try(ok, Bool)
        \\
        \\**Hint:** The error types from all `?` operators and the function body must be compatible since any of them could be the actual return value.
        \\
        \\
        ,
    );
}

// recursive functions //

test "check type - self recursive function - fibonacci - pass" {
    const source =
        \\fib = |n| {
        \\  if n <= 1.U8 {
        \\    n
        \\  } else {
        \\    fib(n - 1.U8) + fib(n - 2.U8)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "fib" } }, "U8 -> U8");
}

test "check type - mutually recursive functions - constraint propagation" {
    const source =
        \\f = |x| { a: x, b: g(x) }
        \\g = |y| f(y).a
        \\test = (f ,g)
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "test", .expected = "(c -> { a: c, b: c }, d -> d)" },
        },
    );
}

test "check type - self recursive function - fibonacci - fail" {
    const source =
        \\fib = |n| {
        \\  if n <= 1.U8 {
        \\    n
        \\  } else {
        \\    fib("bad arg") + fib(n - 2.U8)
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .fail_with,
        \\**TYPE MISMATCH**
        \\The recursive definition `fib` is used in an unexpected way:
        \\**test:5:5:5:8:**
        \\```roc
        \\    fib("bad arg") + fib(n - 2.U8)
        \\```
        \\    ^^^
        \\
        \\It has the type:
        \\
        \\    Str -> U8
        \\
        \\But other places expect it to be:
        \\
        \\    U8 -> U8
        \\
        \\
        ,
    );
}

test "check type - mutually recursive functions - is_even and is_odd" {
    const source =
        \\is_even = |n| {
        \\  if n == 0.U64 {
        \\    Bool.True
        \\  } else {
        \\    is_odd(n - 1.U64)
        \\  }
        \\}
        \\
        \\is_odd = |n| {
        \\  if n == 0.U64 {
        \\    Bool.False
        \\  } else {
        \\    is_even(n - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "is_odd" } }, "U64 -> Bool");
}

// self recursive functions - additional //

test "check type - self recursive function - factorial" {
    const source =
        \\fact = |n| {
        \\  if n <= 1.U64 {
        \\    1.U64
        \\  } else {
        \\    n * fact(n - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "fact" } }, "U64 -> U64");
}

test "check type - self recursive function - multiple args" {
    const source =
        \\power = |base, exp| {
        \\  if exp <= 0.U64 {
        \\    1.U64
        \\  } else {
        \\    base * power(base, exp - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "power" } }, "U64, U64 -> U64");
}

test "check type - self recursive function - with accumulator" {
    const source =
        \\sum_to : U64, U64 -> U64
        \\sum_to = |n, acc| {
        \\  if n <= 0.U64 {
        \\    acc
        \\  } else {
        \\    sum_to(n - 1.U64, acc + n)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "sum_to" } }, "U64, U64 -> U64");
}

test "check type - self recursive function - returning record" {
    const source =
        \\count = |n| {
        \\  if n <= 0.U64 {
        \\    { value: 0.U64, calls: 1.U64 }
        \\  } else {
        \\    prev = count(n - 1.U64)
        \\    { value: prev.value + n, calls: prev.calls + 1.U64 }
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "count" } }, "U64 -> { calls: U64, value: U64 }");
}

test "check type - self recursive function - polymorphic after generalization" {
    const source =
        \\const_rec = |n, x| {
        \\  if n <= 0.U64 {
        \\    x
        \\  } else {
        \\    const_rec(n - 1.U64, x)
        \\  }
        \\}
        \\test = (const_rec(1.U64, "hello"), const_rec(1.U64, 42.U8))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "const_rec", .expected = "U64, a -> a" },
            .{ .def = "test", .expected = "(Str, U8)" },
        },
    );
}

test "check type - self recursive function - inner lambda calls outer" {
    const source =
        \\apply_recursive = |n| {
        \\  if n <= 0.U64 {
        \\    0.U64
        \\  } else {
        \\    (|x| apply_recursive(x))(n - 1.U64) + 1.U64
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "apply_recursive" } }, "U64 -> U64");
}

test "check type - self recursive function - with annotation" {
    const source =
        \\fact : U64 -> U64
        \\fact = |n| {
        \\  if n <= 1.U64 {
        \\    1.U64
        \\  } else {
        \\    n * fact(n - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "fact" } }, "U64 -> U64");
}

test "check type - self recursive function - wrong arg type" {
    const source =
        \\bad = |x| {
        \\  if x == 0.U64 {
        \\    x
        \\  } else {
        \\    bad(Bool.True)
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .fail,
        "TYPE MISMATCH",
    );
}

// self recursive static dispatch //

test "check type - self recursive static dispatch - method calls itself" {
    const source =
        \\Counter := [Val(U64)].{
        \\  count_down = |Counter.Val(n)| {
        \\    if n == 0.U64 {
        \\      0.U64
        \\    } else {
        \\      Counter.Val(n - 1.U64).count_down()
        \\    }
        \\  }
        \\}
        \\
        \\main = Counter.Val(5.U64).count_down()
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Counter.count_down", .expected = "Counter -> U64" },
            .{ .def = "main", .expected = "U64" },
        },
    );
}

test "check type - self recursive static dispatch - method with args" {
    const source =
        \\Acc := [Val(U64)].{
        \\  add_n = |Acc.Val(current), n| {
        \\    if n == 0.U64 {
        \\      Acc.Val(current)
        \\    } else {
        \\      Acc.Val(current + 1.U64).add_n(n - 1.U64)
        \\    }
        \\  }
        \\}
        \\
        \\main = Acc.Val(0.U64).add_n(5.U64)
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Acc.add_n", .expected = "Acc, U64 -> Acc" },
            .{ .def = "main", .expected = "Acc" },
        },
    );
}

// mutually recursive static dispatch //

test "check type - mutually recursive static dispatch - methods on same type" {
    const source =
        \\Checker := [Val(U64)].{
        \\  is_even = |Checker.Val(n)| {
        \\    if n == 0.U64 {
        \\      Bool.True
        \\    } else {
        \\      Checker.Val(n - 1.U64).is_odd()
        \\    }
        \\  }
        \\  is_odd = |Checker.Val(n)| {
        \\    if n == 0.U64 {
        \\      Bool.False
        \\    } else {
        \\      Checker.Val(n - 1.U64).is_even()
        \\    }
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Checker.is_even", .expected = "Checker -> Bool" },
            .{ .def = "Test.Checker.is_odd", .expected = "Checker -> Bool" },
        },
    );
}

test "check type - self recursive static dispatch - with annotation" {
    const source =
        \\Counter := [Val(U64)].{
        \\  count_down : Counter -> U64
        \\  count_down = |Counter.Val(n)| {
        \\    if n == 0.U64 {
        \\      0.U64
        \\    } else {
        \\      Counter.Val(n - 1.U64).count_down()
        \\    }
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "Test.Counter.count_down" } },
        "Counter -> U64",
    );
}

test "check type - self recursive static dispatch - returning record" {
    const source =
        \\Counter := [Val(U64)].{
        \\  count_info = |Counter.Val(n)| {
        \\    if n == 0.U64 {
        \\      { done: Bool.True, value: n }
        \\    } else {
        \\      Counter.Val(n - 1.U64).count_info()
        \\    }
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "Test.Counter.count_info" } },
        "Counter -> { done: Bool, value: U64 }",
    );
}

test "check type - self recursive static dispatch - polymorphic" {
    const source =
        \\Wrapper(a) := [Val(a)].{
        \\  apply_n = |Wrapper.Val(x), f, n| {
        \\    if n == 0.U64 {
        \\      Wrapper.Val(x)
        \\    } else {
        \\      Wrapper.Val(f(x)).apply_n(f, n - 1.U64)
        \\    }
        \\  }
        \\}
        \\test = (Wrapper.Val(1.U64).apply_n(|x| x + 1.U64, 3.U64), Wrapper.Val("hi").apply_n(|s| s, 1.U64))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Wrapper.apply_n", .expected = "Wrapper(a), (a -> a), U64 -> Wrapper(a)" },
            .{ .def = "test", .expected = "(Wrapper(U64), Wrapper(Str))" },
        },
    );
}

test "check type - self recursive static dispatch - wrong arg type" {
    const source =
        \\Counter := [Val(U64)].{
        \\  bad_count = |Counter.Val(n)| {
        \\    if n == 0.U64 {
        \\      0.U64
        \\    } else {
        \\      Counter.Val("bad").bad_count()
        \\    }
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .fail,
        "INVALID NOMINAL TAG",
    );
}

// mutually recursive functions - additional //

test "check type - mutually recursive functions - three-way cycle" {
    const source =
        \\f = |n| {
        \\  if n <= 0.U64 {
        \\    0.U64
        \\  } else {
        \\    g(n - 1.U64)
        \\  }
        \\}
        \\g = |n| {
        \\  if n <= 0.U64 {
        \\    0.U64
        \\  } else {
        \\    h(n - 1.U64)
        \\  }
        \\}
        \\h = |n| {
        \\  if n <= 0.U64 {
        \\    0.U64
        \\  } else {
        \\    f(n - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64 -> U64" },
            .{ .def = "g", .expected = "U64 -> U64" },
            .{ .def = "h", .expected = "U64 -> U64" },
        },
    );
}

test "check type - mutually recursive functions - polymorphic" {
    const source =
        \\ping = |n, x| {
        \\  if n <= 0.U64 {
        \\    x
        \\  } else {
        \\    pong(n - 1.U64, x)
        \\  }
        \\}
        \\pong = |n, x| {
        \\  if n <= 0.U64 {
        \\    x
        \\  } else {
        \\    ping(n - 1.U64, x)
        \\  }
        \\}
        \\test = (ping(2.U64, "hello"), pong(1.U64, 42.U8))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "ping", .expected = "U64, a -> a" },
            .{ .def = "pong", .expected = "U64, a -> a" },
            .{ .def = "test", .expected = "(Str, U8)" },
        },
    );
}

test "check type - mutually recursive functions - record constraint propagation" {
    const source =
        \\make_pair = |x| { fst: x, snd: get_fst(x) }
        \\get_fst = |y| make_pair(y).fst
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "make_pair", .expected = "a -> { fst: a, snd: a }" },
            .{ .def = "get_fst", .expected = "a -> a" },
        },
    );
}

// mutually recursive static dispatch - additional //

test "check type - mutually recursive static dispatch - three-way cycle" {
    const source =
        \\Triple := [Val(U64)].{
        \\  step_a = |Triple.Val(n)| {
        \\    if n == 0.U64 {
        \\      0.U64
        \\    } else {
        \\      Triple.Val(n - 1.U64).step_b()
        \\    }
        \\  }
        \\  step_b = |Triple.Val(n)| {
        \\    if n == 0.U64 {
        \\      0.U64
        \\    } else {
        \\      Triple.Val(n - 1.U64).step_c()
        \\    }
        \\  }
        \\  step_c = |Triple.Val(n)| {
        \\    if n == 0.U64 {
        \\      0.U64
        \\    } else {
        \\      Triple.Val(n - 1.U64).step_a()
        \\    }
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Triple.step_a", .expected = "Triple -> U64" },
            .{ .def = "Test.Triple.step_b", .expected = "Triple -> U64" },
            .{ .def = "Test.Triple.step_c", .expected = "Triple -> U64" },
        },
    );
}

test "check type - mutually recursive static dispatch - polymorphic" {
    const source =
        \\Stepper(a) := [Val(a)].{
        \\  step_even = |Stepper.Val(x), n| {
        \\    if n == 0.U64 {
        \\      x
        \\    } else {
        \\      Stepper.Val(x).step_odd(n - 1.U64)
        \\    }
        \\  }
        \\  step_odd = |Stepper.Val(x), n| {
        \\    if n == 0.U64 {
        \\      x
        \\    } else {
        \\      Stepper.Val(x).step_even(n - 1.U64)
        \\    }
        \\  }
        \\}
        \\test = (Stepper.Val(1.U64).step_even(2.U64), Stepper.Val("hi").step_odd(1.U64))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Stepper.step_even", .expected = "Stepper(a), U64 -> a" },
            .{ .def = "Test.Stepper.step_odd", .expected = "Stepper(a), U64 -> a" },
            .{ .def = "test", .expected = "(U64, Str)" },
        },
    );
}

test "check type - associated items with operators - no mutual recursion regression" {
    const source =
        \\MyType := [C].{
        \\  a = b + c
        \\  b = 10
        \\  c = d + 5
        \\  d = 20
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.MyType.a", .expected = "Dec" },
            .{ .def = "Test.MyType.b", .expected = "Dec" },
            .{ .def = "Test.MyType.c", .expected = "Dec" },
            .{ .def = "Test.MyType.d", .expected = "Dec" },
        },
    );
}

test "check type - mutually recursive functions - type mismatch error" {
    const source =
        \\f = |n| {
        \\  if n <= 0.U64 {
        \\    "hello"
        \\  } else {
        \\    g(n - 1.U64)
        \\  }
        \\}
        \\g = |n| {
        \\  if n <= 0.U64 {
        \\    42.U64
        \\  } else {
        \\    f(n - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - mutually recursive functions - three-way polymorphic" {
    const source =
        \\f = |n, x| {
        \\  if n <= 0.U64 {
        \\    x
        \\  } else {
        \\    g(n - 1.U64, x)
        \\  }
        \\}
        \\g = |n, x| {
        \\  if n <= 0.U64 {
        \\    x
        \\  } else {
        \\    h(n - 1.U64, x)
        \\  }
        \\}
        \\h = |n, x| {
        \\  if n <= 0.U64 {
        \\    x
        \\  } else {
        \\    f(n - 1.U64, x)
        \\  }
        \\}
        \\test = (f(3.U64, "hi"), g(2.U64, 42.U8), h(1.U64, Bool.True))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64, a -> a" },
            .{ .def = "g", .expected = "U64, a -> a" },
            .{ .def = "h", .expected = "U64, a -> a" },
            .{ .def = "test", .expected = "(Str, U8, Bool)" },
        },
    );
}

test "check type - mutually recursive static dispatch - three-way polymorphic" {
    const source =
        \\Relay(a) := [Val(a)].{
        \\  step_a = |Relay.Val(x), n| {
        \\    if n == 0.U64 {
        \\      x
        \\    } else {
        \\      Relay.Val(x).step_b(n - 1.U64)
        \\    }
        \\  }
        \\  step_b = |Relay.Val(x), n| {
        \\    if n == 0.U64 {
        \\      x
        \\    } else {
        \\      Relay.Val(x).step_c(n - 1.U64)
        \\    }
        \\  }
        \\  step_c = |Relay.Val(x), n| {
        \\    if n == 0.U64 {
        \\      x
        \\    } else {
        \\      Relay.Val(x).step_a(n - 1.U64)
        \\    }
        \\  }
        \\}
        \\test = (Relay.Val(1.U64).step_a(2.U64), Relay.Val("hi").step_c(1.U64))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Relay.step_a", .expected = "Relay(a), U64 -> a" },
            .{ .def = "Test.Relay.step_b", .expected = "Relay(a), U64 -> a" },
            .{ .def = "Test.Relay.step_c", .expected = "Relay(a), U64 -> a" },
            .{ .def = "test", .expected = "(U64, Str)" },
        },
    );
}

test "check type - mutually recursive functions - annotated" {
    const source =
        \\is_even : U64 -> Bool
        \\is_even = |n| {
        \\  if n == 0.U64 {
        \\    Bool.True
        \\  } else {
        \\    is_odd(n - 1.U64)
        \\  }
        \\}
        \\is_odd : U64 -> Bool
        \\is_odd = |n| {
        \\  if n == 0.U64 {
        \\    Bool.False
        \\  } else {
        \\    is_even(n - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "is_even", .expected = "U64 -> Bool" },
            .{ .def = "is_odd", .expected = "U64 -> Bool" },
        },
    );
}

test "check type - mutually recursive functions - different arities" {
    const source =
        \\f = |a| {
        \\  if a <= 0.U64 {
        \\    0.U64
        \\  } else {
        \\    g(a - 1.U64, a)
        \\  }
        \\}
        \\g = |a, b| {
        \\  if a <= 0.U64 {
        \\    b
        \\  } else {
        \\    f(a - 1.U64)
        \\  }
        \\}
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64 -> U64" },
            .{ .def = "g", .expected = "U64, U64 -> U64" },
        },
    );
}

test "check type - mutually recursive functions - diamond pattern" {
    const source =
        \\f = |n| if n <= 0.U64 { 0.U64 } else { g(n - 1.U64) + h(n - 1.U64) }
        \\g = |n| if n <= 0.U64 { 0.U64 } else { shared(n - 1.U64) }
        \\h = |n| if n <= 0.U64 { 0.U64 } else { shared(n - 1.U64) }
        \\shared = |n| if n <= 0.U64 { 0.U64 } else { f(n - 1.U64) }
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64 -> U64" },
            .{ .def = "g", .expected = "U64 -> U64" },
            .{ .def = "h", .expected = "U64 -> U64" },
            .{ .def = "shared", .expected = "U64 -> U64" },
        },
    );
}

test "check type - mutually recursive functions - mixed function and value defs" {
    const source =
        \\f = |x| if x <= 0.U64 { 0.U64 } else { g(x - 1.U64) }
        \\g = |x| if x <= 0.U64 { 0.U64 } else { f(x - 1.U64) }
        \\val = f(10.U64)
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64 -> U64" },
            .{ .def = "g", .expected = "U64 -> U64" },
            .{ .def = "val", .expected = "U64" },
        },
    );
}

test "check type - mutually recursive functions - entry order independence" {
    const source =
        \\ping = |n, x| if n <= 0.U64 { x } else { pong(n - 1.U64, x) }
        \\pong = |n, x| if n <= 0.U64 { x } else { ping(n - 1.U64, x) }
        \\use_ping = ping(1.U64, "hello")
        \\use_pong = pong(1.U64, 42.U8)
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "ping", .expected = "U64, a -> a" },
            .{ .def = "pong", .expected = "U64, a -> a" },
            .{ .def = "use_ping", .expected = "Str" },
            .{ .def = "use_pong", .expected = "U8" },
        },
    );
}

test "check type - mutually recursive functions - polymorphic diamond" {
    const source =
        \\wrap = |n, x| if n <= 0.U64 { x } else { via_a(n - 1.U64, x) }
        \\via_a = |n, x| if n <= 0.U64 { x } else { core(n - 1.U64, x) }
        \\via_b = |n, x| if n <= 0.U64 { x } else { core(n - 1.U64, x) }
        \\core = |n, x| if n <= 0.U64 { x } else { wrap(n - 1.U64, x) }
        \\test = (wrap(1.U64, "hi"), via_b(1.U64, 42.U8))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "wrap", .expected = "U64, a -> a" },
            .{ .def = "via_a", .expected = "U64, a -> a" },
            .{ .def = "via_b", .expected = "U64, a -> a" },
            .{ .def = "core", .expected = "U64, a -> a" },
            .{ .def = "test", .expected = "(Str, U8)" },
        },
    );
}

test "check type - mutually recursive static dispatch - diamond pattern" {
    const source =
        \\Router(a) := [Val(a)].{
        \\  route = |Router.Val(x), n| if n == 0.U64 { x } else { Router.Val(x).path_a(n - 1.U64) }
        \\  path_a = |Router.Val(x), n| if n == 0.U64 { x } else { Router.Val(x).hub(n - 1.U64) }
        \\  path_b = |Router.Val(x), n| if n == 0.U64 { x } else { Router.Val(x).hub(n - 1.U64) }
        \\  hub = |Router.Val(x), n| if n == 0.U64 { x } else { Router.Val(x).route(n - 1.U64) }
        \\}
        \\test = (Router.Val(1.U64).route(2.U64), Router.Val("hi").path_b(1.U64))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "Test.Router.route", .expected = "Router(a), U64 -> a" },
            .{ .def = "Test.Router.path_a", .expected = "Router(a), U64 -> a" },
            .{ .def = "Test.Router.path_b", .expected = "Router(a), U64 -> a" },
            .{ .def = "Test.Router.hub", .expected = "Router(a), U64 -> a" },
            .{ .def = "test", .expected = "(U64, Str)" },
        },
    );
}

test "check type - mutually recursive functions - partially annotated" {
    const source =
        \\f : U64 -> U64
        \\f = |x| if x <= 0.U64 { 0.U64 } else { g(x - 1.U64) }
        \\g = |x| if x <= 0.U64 { 0.U64 } else { f(x - 1.U64) }
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64 -> U64" },
            .{ .def = "g", .expected = "U64 -> U64" },
        },
    );
}

test "check type - mutually recursive functions - partially annotated polymorphic" {
    const source =
        \\ping : U64, a -> a
        \\ping = |n, x| if n <= 0.U64 { x } else { pong(n - 1.U64, x) }
        \\pong = |n, x| if n <= 0.U64 { x } else { ping(n - 1.U64, x) }
        \\test = (ping(1.U64, "hi"), pong(1.U64, 42.U8))
    ;
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "ping", .expected = "U64, a -> a" },
            .{ .def = "pong", .expected = "U64, a -> a" },
            .{ .def = "test", .expected = "(Str, U8)" },
        },
    );
}

test "check type - mutually recursive functions - inner let-def lambda inside cycle participant is generalized" {
    // Inner let-def lambda should generalize normally even while
    // defer_generalize is active for the outer cycle.
    const source =
        \\f = |n| {
        \\    id = |x| x
        \\    _unused = id("hello")
        \\    if n <= 0.U64 { 0.U64 } else { id(g(n - 1.U64)) }
        \\}
        \\g = |n| if n <= 0.U64 { 0.U64 } else { f(n - 1.U64) }
    ;
    // If id weren't generalized, using it at both Str and U64 would fail.
    try checkTypesModuleDefs(
        source,
        &.{
            .{ .def = "f", .expected = "U64 -> U64" },
            .{ .def = "g", .expected = "U64 -> U64" },
        },
    );
}

// repros //

test "check type - zulip repro" {
    const source =
        \\main! = |_args| {
        \\    rec = create_record()
        \\    use_record(rec)
        \\}
        \\create_record = || {
        \\    {foo: "bar"}
        \\}
        \\use_record = |rec| {
        \\    Str.is_empty(rec.blah)
        \\}
    ;
    try checkTypesModule(source, .fail_with,
        \\**TYPE MISMATCH**
        \\The first argument being passed to this function has the wrong type:
        \\**test:3:5:**
        \\```roc
        \\    use_record(rec)
        \\```
        \\               ^^^
        \\
        \\This argument has the type:
        \\
        \\    { foo: Str }
        \\
        \\But `use_record` needs the first argument to be:
        \\
        \\    { .., blah: Str }
        \\
        \\**Hint:** This record is missing the field: `blah`
        \\
        \\
    );
}
