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

test "cross-module type checking - monomorphic function passes" {
    const source_a =
        \\module [id_str] 
        \\
        \\id_str : Str -> Str
        \\id_str = |s| s
    ;
    var test_env_a = try TestEnv.init(source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\module [] 
        \\
        \\import A
        \\
        \\main : Str
        \\main = A.id_str("hello")
    ;
    var test_env_b = try TestEnv.initWithImport(source_b, "A", test_env_a.module_env);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Str");
}

test "cross-module type checking - monomorphic function fails" {
    const source_a =
        \\module [id_str] 
        \\
        \\id_str : Str -> Str
        \\id_str = |s| s
    ;
    var test_env_a = try TestEnv.init(source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("Str -> Str");

    const source_b =
        \\module [] 
        \\
        \\import A
        \\
        \\main : U8
        \\main = A.id_str(1)
    ;
    var test_env_b = try TestEnv.initWithImport(source_b, "A", test_env_a.module_env);
    defer test_env_b.deinit();
    try test_env_b.assertOneTypeError("TYPE MISMATCH");
}

test "cross-module type checking - polymorphic function passes" {
    const source_a =
        \\module [id] 
        \\
        \\id : a -> a
        \\id = |s| s
    ;
    var test_env_a = try TestEnv.init(source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\module [] 
        \\
        \\import A
        \\
        \\main : Str
        \\main = A.id("hello")
    ;
    var test_env_b = try TestEnv.initWithImport(source_b, "A", test_env_a.module_env);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Str");
}

test "cross-module type checking - polymorphic function with mulitple uses passes" {
    const source_a =
        \\module [id] 
        \\
        \\id : a -> a
        \\id = |s| s
    ;
    var test_env_a = try TestEnv.init(source_a);
    defer test_env_a.deinit();
    try test_env_a.assertLastDefType("a -> a");

    const source_b =
        \\module [] 
        \\
        \\import A
        \\
        \\main : U64
        \\main = {
        \\  a =  A.id(10)
        \\  b =  A.id(15)
        \\  _c =  A.id("Hello")
        \\  a + b
        \\}
    ;
    var test_env_b = try TestEnv.initWithImport(source_b, "A", test_env_a.module_env);
    defer test_env_b.deinit();
    try test_env_b.assertLastDefType("Num(Int(Unsigned64))");
}
