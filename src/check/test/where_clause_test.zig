//! Comprehensive tests for where clause type checking functionality.
//!
//! These tests cover:
//! - Basic where clause parsing and type inference
//! - Method constraints on type variables
//! - Multiple constraints
//! - Constraint satisfaction
//! - Error cases

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

// Basic where clause tests

test "where clause - basic method constraint infers correctly" {
    // Module A defines a type with to_str method
    const source_a =
        \\A := [Val(Str)].{
        \\  to_str : A -> Str
        \\  to_str = |A.Val(s)| s
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    // Module B uses A and defines a helper with where clause
    const source_b =
        \\import A
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main : Str
        \\main = helper(A.Val("hello"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType(
        "helper",
        "a -> Str where [a.to_str : a -> Str]",
    );
    try test_env_b.assertDefType("main", "Str");
}

test "where clause - polymorphic return type" {
    const source_a =
        \\A := [Val(Str)].{
        \\  to_str : A -> Str
        \\  to_str = |A.Val(s)| s
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\helper : a -> b where [a.to_str : a -> b]
        \\helper = |x| x.to_str()
        \\
        \\main : Str
        \\main = helper(A.Val("hello"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType(
        "helper",
        "a -> b where [a.to_str : a -> b]",
    );
    try test_env_b.assertDefType("main", "Str");
}

test "where clause - constraint with multiple args" {
    const source_a =
        \\A := [Box(Str)].{
        \\  transform : A, (Str -> Str) -> A
        \\  transform = |A.Box(s), fn| A.Box(fn(s))
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\modify : a, (Str -> Str) -> a where [a.transform : a, (Str -> Str) -> a]
        \\modify = |x, fn| x.transform(fn)
        \\
        \\main : A
        \\main = modify(A.Box("hello"), |s| s)
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType(
        "modify",
        "a, (Str -> Str) -> a where [a.transform : a, (Str -> Str) -> a]",
    );
    try test_env_b.assertDefType("main", "A");
}

// Multiple constraints tests

test "where clause - multiple constraints on same variable" {
    const source_a =
        \\A := [D(Str, U64)].{
        \\  to_str : A -> Str
        \\  to_str = |A.D(s, _)| s
        \\
        \\  to_u64 : A -> U64
        \\  to_u64 = |A.D(_, n)| n
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\both : a -> (Str, U64) where [a.to_str : a -> Str, a.to_u64 : a -> U64]
        \\both = |x| (x.to_str(), x.to_u64())
        \\
        \\main : (Str, U64)
        \\main = both(A.D("hello", 42))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType(
        "both",
        "a -> (Str, U64) where [a.to_str : a -> Str, a.to_u64 : a -> U64]",
    );
    try test_env_b.assertDefType("main", "(Str, U64)");
}

// Cross-module where clause tests

test "where clause - cross-module constraint satisfaction" {
    const source_a =
        \\A := [A(Str)].{
        \\  to_str : A -> Str
        \\  to_str = |A.A(val)| val
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("A.to_str", "A -> Str");

    const source_b =
        \\import A
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main : Str
        \\main = helper(A.A("hello"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType(
        "helper",
        "a -> Str where [a.to_str : a -> Str]",
    );
    try test_env_b.assertDefType("main", "Str");
}

test "where clause - cross-module polymorphic constraint" {
    const source_a =
        \\A := [A(Str)].{
        \\  to_str = |A.A(val)| val
        \\  to_str2 = |x| x.to_str()
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("A.to_str", "A -> Str");
    try test_env_a.assertDefType(
        "A.to_str2",
        "a -> b where [a.to_str : a -> b]",
    );

    const source_b =
        \\import A
        \\
        \\main : Str
        \\main = (A.A("hello")).to_str2()
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("main", "Str");
}

// Nested/chained where clause tests

test "where clause - chained method calls" {
    const source_a =
        \\A := [Box(Str)].{
        \\  get_value : A -> Str
        \\  get_value = |A.Box(s)| s
        \\
        \\  transform : A, (Str -> Str) -> A
        \\  transform = |A.Box(s), fn| A.Box(fn(s))
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\chain : a, (Str -> Str) -> Str where [a.transform : a, (Str -> Str) -> a, a.get_value : a -> Str]
        \\chain = |x, fn| x.transform(fn).get_value()
        \\
        \\main : Str
        \\main = chain(A.Box("hello"), |s| s)
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("main", "Str");
}

// Error case tests

test "where clause - missing method on type" {
    const source_a =
        \\A := [Val(Str)].{}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main = helper(A.Val("hello"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertFirstTypeError("MISSING METHOD");
}

test "where clause - method signature mismatch" {
    const source_a =
        \\A := [Val(Str)].{
        \\  to_str : A -> U64
        \\  to_str = |_| 42
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main = helper(A.Val("hello"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertFirstTypeError("TYPE MISMATCH");
}

// Let polymorphism with where clauses

test "where clause - same type used multiple times with where constraint" {
    const source_a =
        \\A := [A(Str)].{
        \\  to_str : A -> Str
        \\  to_str = |A.A(s)| s
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\helper : a -> Str where [a.to_str : a -> Str]
        \\helper = |x| x.to_str()
        \\
        \\main : (Str, Str)
        \\main = (helper(A.A("hello")), helper(A.A("world")))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("main", "(Str, Str)");
}

// Where clause without annotation (inferred)

test "where clause - inferred from method call without annotation" {
    const source_a =
        \\A := [Val(Str)].{
        \\  to_str : A -> Str
        \\  to_str = |A.Val(s)| s
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\helper = |x| x.to_str()
        \\
        \\main : Str
        \\main = helper(A.Val("hello"))
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType(
        "helper",
        "a -> b where [a.to_str : a -> b]",
    );
    try test_env_b.assertDefType("main", "Str");
}
