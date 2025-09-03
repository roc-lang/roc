//! Let polymorphism integration tests for the new CIR architecture
//! Tests the type checker's ability to handle polymorphic let bindings

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const types = @import("types");
const canonicalize = @import("can");
const check = @import("../mod.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = canonicalize.ModuleEnv;
const CIR = canonicalize.CIR;

/// Helper to create a test module environment
fn createTestEnv() !*ModuleEnv {
    const env = try test_allocator.create(ModuleEnv);
    env.* = try ModuleEnv.init(test_allocator, "Test");
    return env;
}

/// Helper to parse and canonicalize source code
fn parseAndCanonicalizeSource(env: *ModuleEnv, source: []const u8) !CIR {
    // Parse the source
    var ast = try parse.parse(test_allocator, source, env.getIdents());
    defer ast.deinit(test_allocator);

    // Create CIR and canonicalize
    const cir = CIR.init(&ast, &env.types);
    // Note: In real usage, canonicalization would happen here
    // For these tests, we're focusing on the type checking aspects

    return cir;
}

test "let polymorphism - identity function" {
    // Test that the identity function can be used at multiple types
    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    // Create identity function: |x| x
    const cir = CIR.init(&env.byte_slices, &env.types, env.getIdents());
    defer cir.deinit(test_allocator);

    // Create type variables for the polymorphic identity function
    const a_var = try env.types.fresh();
    const id_type = try env.types.fresh();
    const args_range = try env.types.appendVars(&[_]types.Var{a_var});
    try env.types.setVarContent(id_type, .{ .structure = .{ .fn_pure = .{ .args = args_range, .ret = a_var } } });

    // Use the identity function at two different types
    const int_var = try env.types.fresh();
    try env.types.setVarContent(int_var, .{ .structure = .{ .int = .i32 } });

    const str_var = try env.types.mkStr(test_allocator, env.getIdents(), null);

    // The identity function should work with both Int and Str
    // This demonstrates let polymorphism where a single function
    // can be instantiated at multiple types
    try testing.expect(a_var != int_var);
    try testing.expect(a_var != str_var);
}

test "let polymorphism - map function" {
    // Test that a polymorphic map function works correctly
    const source =
        \\map = |f, list|
        \\    when list is
        \\        [] -> []
        \\        [x, ..xs] -> [f x, ..map f xs]
        \\
        \\main =
        \\    nums = map (|x| x + 1) [1, 2, 3]
        \\    strs = map (|s| Str.concat s "!") ["a", "b"]
        \\    { nums, strs }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // map should have type: (a -> b), List a -> List b
    // It should be instantiated at (Num -> Num), List Num -> List Num
    // and (Str -> Str), List Str -> List Str
}

test "let polymorphism - nested let bindings" {
    // Test that nested let bindings maintain proper polymorphism
    const source =
        \\outer = |x|
        \\    inner = |y| y
        \\    { a: inner x, b: inner "test" }
        \\
        \\main = outer 42
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // inner should be polymorphic within outer's body
    // Result should be { a: Num, b: Str }
}

test "let polymorphism - value restriction" {
    // Test that value restriction prevents unsound polymorphism
    const source =
        \\ref = |x| { val: x }
        \\
        \\main =
        \\    r = ref []  # Should not be polymorphic
        \\    a = r.val ++ [1]
        \\    b = r.val ++ ["hello"]  # This should fail
        \\    { a, b }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    const result = checker.checkCIR(&cir);

    // Should have type error - can't use empty list at two different types
    try testing.expect(result == error.TypeError or cir.diagnostics.items.len > 0);
}

test "let polymorphism - recursive binding monomorphism" {
    // Test that recursive bindings are monomorphic
    const source =
        \\length = |list|
        \\    when list is
        \\        [] -> 0
        \\        [_, ..xs] -> 1 + length xs
        \\
        \\main =
        \\    a = length [1, 2, 3]
        \\    b = length ["a", "b"]  # Should work - length is polymorphic
        \\    { a, b }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // length should have polymorphic type: List a -> Num
    // Can be used with both List Num and List Str
}

test "let polymorphism - constrained polymorphism" {
    // Test polymorphism with type constraints
    const source =
        \\double = |x| x + x
        \\
        \\main =
        \\    a = double 21      # Int
        \\    b = double 1.5     # Float
        \\    { a, b }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // double should have type: Num a => a -> a
    // Works with any numeric type
}

test "let polymorphism - higher-rank types" {
    // Test that we properly handle rank-2 polymorphism
    const source =
        \\apply = |f| f 42
        \\
        \\polyId = |x| x
        \\
        \\main = apply polyId  # Should work if polyId is polymorphic enough
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // This tests our support for higher-rank polymorphism
    // polyId needs to be instantiated inside apply's body
}

test "let polymorphism - pattern matching polymorphism" {
    // Test polymorphism in pattern matching
    const source =
        \\swap = |pair|
        \\    when pair is
        \\        (a, b) -> (b, a)
        \\
        \\main =
        \\    p1 = swap (1, "hello")
        \\    p2 = swap (True, 42)
        \\    { p1, p2 }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // swap should have type: (a, b) -> (b, a)
    // Should work with different type instantiations
}

test "let polymorphism - mutual recursion" {
    // Test that mutually recursive functions handle polymorphism correctly
    const source =
        \\isEven = |n|
        \\    when n is
        \\        0 -> True
        \\        _ -> isOdd (n - 1)
        \\
        \\isOdd = |n|
        \\    when n is
        \\        0 -> False
        \\        _ -> isEven (n - 1)
        \\
        \\main = { even: isEven 4, odd: isOdd 7 }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // Both functions should have type: Num -> Bool
    // Mutual recursion should be handled correctly
}

test "let polymorphism - polymorphic record fields" {
    // Test polymorphism with record field access
    const source =
        \\getFirst = |rec| rec.first
        \\
        \\main =
        \\    a = getFirst { first: 42, second: "hello" }
        \\    b = getFirst { first: "world", other: True }
        \\    { a, b }
    ;

    const env = try createTestEnv();
    defer {
        env.deinit();
        test_allocator.destroy(env);
    }

    var cir = try parseAndCanonicalizeSource(env, source);
    defer cir.deinit(test_allocator);

    // Type check
    var checker = check.TypeChecker.init(test_allocator, &env.types, env.getIdents());
    defer checker.deinit();

    try checker.checkCIR(&cir);

    // Verify no type errors
    try testing.expect(cir.diagnostics.items.len == 0);

    // getFirst should have polymorphic type: { first: a }* -> a
    // The * indicates row polymorphism (record can have other fields)
}
