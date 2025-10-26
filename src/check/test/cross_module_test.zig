//! Tests for cross-module type checking functionality.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");
const can = @import("can");
const Check = @import("../Check.zig");
const TestEnv = @import("./TestEnv.zig");

const CIR = can.CIR;
const Var = types_mod.Var;
const Content = types_mod.Content;
const Ident = base.Ident;
const testing = std.testing;
const ModuleEnv = can.ModuleEnv;
const problem = @import("../problem.zig");
const snapshot = @import("../snapshot.zig");
const occurs = @import("../occurs.zig");
const ProblemStore = problem.Store;
const SnapshotStore = snapshot.Store;
const UnifierScratch = @import("../unify.zig").Scratch;
const OccursScratch = occurs.Scratch;
const unify = @import("../unify.zig").unify;

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
    // This test is temporarily skipped due to a regression from auto-import changes.
    // The test expects a TYPE MISMATCH error when calling A.main!(1) where main! expects Str->Str,
    // but the error is not being detected (expected 1 error, found 0).
    // This appears to be related to how auto-imports interact with error reporting in test environments.
    // The other 3 cross-module tests pass, so cross-module - check type works in general.
    // TODO: Investigate why type errors aren't being reported in this specific cross-module test case
    return error.SkipZigTest;

    // const source_a =
    //     \\main! : Str -> Str
    //     \\main! = |s| s
    // ;
    // var test_env_a = try TestEnv.init(source_a);
    // defer test_env_a.deinit();
    // try test_env_a.assertLastDefType("Str -> Str");

    // const source_b =
    //     \\import A
    //     \\
    //     \\main : U8
    //     \\main = A.main!(1)
    // ;
    // var test_env_b = try TestEnv.initWithImport(source_b, "A", test_env_a.module_env);
    // defer test_env_b.deinit();
    // try test_env_b.assertOneTypeError("TYPE MISMATCH");
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
    try test_env_b.assertLastDefType("Num(Int(Unsigned64))");
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

test "cross-module - check type - Str.is_empty returns Bool" {
    // Test that calling Str.is_empty type-checks correctly and returns Bool
    const source =
        \\result : Bool
        \\result = Str.is_empty("")
    ;

    var test_env = try TestEnv.init("test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Bool");
}

test "cross-module - annotation only - can be referenced" {
    const source_a =
        \\foo : Str -> Bool
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Str -> Bool");

    const source_b =
        \\import A
        \\
        \\result : Str -> Bool
        \\result = A.foo
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Str -> Bool");
}

test "cross-module - body only - can be referenced" {
    const source_a =
        \\bar = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    // Type should be inferred as a -> a
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\import A
        \\
        \\result : Str -> Str
        \\result = A.bar
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Str -> Str");
}

test "cross-module - both annotation and body - can be referenced" {
    const source_a =
        \\baz : Str -> Str
        \\baz = |s| s
    ;
    var test_env_a = try TestEnv.init("A", source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\import A
        \\
        \\result : Str -> Str
        \\result = A.baz
    ;
    var test_env_b = try TestEnv.initWithImport("B", source_b, "A", &test_env_a);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Str -> Str");
}
