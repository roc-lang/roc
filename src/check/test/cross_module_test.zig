//! Tests for cross-module type checking functionality.

const std = @import("std");
const Check = @import("../Check.zig");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

test "cross-module - check type - monomorphic function passes" {
    const source_a =
        \\main! : Str -> Str
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    // Str is auto-imported from Builtin module, so it prints as "Str"
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\import A
        \\
        \\main : Str
        \\main = A.main!("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    // Str is auto-imported from Builtin module, so it prints as "Str"
    try test_env_b.assertLastDefType("Str");
}

test "cross-module - check type - monomorphic function fails" {
    const source_a =
        \\main! : Str -> Str
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\import A
        \\
        \\main : U8
        \\main = A.main!(1)
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertOneTypeError("TYPE MISMATCH");
}

test "cross-module - check type - polymorphic function passes" {
    const source_a =
        \\main! : a -> a
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\import A
        \\
        \\main : Str
        \\main = A.main!("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    // Str is auto-imported from Builtin module, so it prints as "Str"
    try test_env_b.assertLastDefType("Str");
}

test "cross-module - check type - polymorphic function with multiple uses passes" {
    const source_a =
        \\main! : a -> a
        \\main! = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\import A
        \\
        \\main : U64
        \\main = {
        \\  a =  A.main!(10)
        \\  b =  A.main!(15)
        \\  _c =  A.main!("Hello")
        \\  a + b
        \\}
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("U64");
}

test "cross-module - check type - static dispatch" {
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
        \\a_val = A.A("hello")
        \\
        \\main = a_val.to_str()
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("a_val", "A");
    try test_env_b.assertDefType("main", "Str");
}

test "cross-module - check type - static dispatch - no annotation & indirection" {
    const source_a =
        \\A := [A(Str)].{
        \\  to_str = |A.A(val)| val
        \\  to_str2 = |x| x.to_str()
        \\}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertDefType("A.to_str", "A -> Str");
    try test_env_a.assertDefType("A.to_str2", "a -> b where [a.to_str : a -> b]");

    const source_b =
        \\import A
        \\
        \\val1 = A.A("hello")
        \\val2 = A.A("world")
        \\
        \\main = (val1.to_str(), val1.to_str2(), val2.to_str2())
        \\
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertDefType("val1", "A");
    try test_env_b.assertDefType("val2", "A");
    try test_env_b.assertDefType("main", "(Str, Str, Str)");
}

test "cross-module - check type - opaque types 1" {
    const source_a =
        \\A :: [A(Str)].{}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\a_val : A.A 
        \\a_val = A("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertFirstTypeError("TYPE MISMATCH");
}

test "cross-module - check type - opaque types 2" {
    const source_a =
        \\A :: [A(Str)].{}
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();

    const source_b =
        \\import A
        \\
        \\a_val = A.A("hello")
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertFirstTypeError("CANNOT USE OPAQUE NOMINAL TYPE");
}

test "displayNameIsBetter - shorter names are preferred" {
    // Tests the core comparison logic used when multiple imports provide different
    // display names for the same type (e.g., `import Foo as F` and `import Foo as Foo`).
    // The shortest name wins for error message display. For equal lengths, the
    // lexicographically smaller name wins (deterministic regardless of import order).
    const displayNameIsBetter = Check.displayNameIsBetter;

    // Shorter is better
    try testing.expect(displayNameIsBetter("T", "Type"));
    try testing.expect(displayNameIsBetter("AB", "ABC"));
    try testing.expect(!displayNameIsBetter("Type", "T"));
    try testing.expect(!displayNameIsBetter("ABC", "AB"));

    // Equal length: lexicographically smaller wins
    try testing.expect(displayNameIsBetter("Abc", "Bbc")); // 'A' < 'B'
    try testing.expect(displayNameIsBetter("Aac", "Abc")); // 'a' < 'b' at position 1
    try testing.expect(!displayNameIsBetter("Bbc", "Abc"));
    try testing.expect(!displayNameIsBetter("Abc", "Aac"));

    // Identical strings: no replacement
    try testing.expect(!displayNameIsBetter("Same", "Same"));
    try testing.expect(!displayNameIsBetter("", ""));
}

test "cross-module - check type - nested module access" {
    // Test access to nested modules across files (issue #9074)
    // When a module Outer contains a nested module Inner with a value inner,
    // accessing Outer.Inner.inner from another file that imports Outer should work.
    const source_outer =
        \\Outer := [].{
        \\    outer : I64
        \\    outer = 20
        \\    Inner := [].{
        \\        inner : I64
        \\        inner = 10
        \\    }
        \\}
    ;
    var test_env_outer = try TestEnv.init("Outer", source_outer);
    defer test_env_outer.deinit();
    try test_env_outer.assertDefType("Outer.outer", "I64");
    try test_env_outer.assertDefType("Outer.Inner.inner", "I64");

    const source_main =
        \\import Outer
        \\
        \\test1 : I64
        \\test1 = Outer.outer
        \\test2 : I64
        \\test2 = Outer.Inner.inner
        \\main : I64
        \\main = test1 + test2
    ;
    var test_env_main = try TestEnv.initWithImport("Main", source_main, "Outer", &test_env_outer);
    defer test_env_main.deinit();
    try test_env_main.assertDefType("test1", "I64");
    try test_env_main.assertDefType("test2", "I64");
    try test_env_main.assertDefType("main", "I64");
}
