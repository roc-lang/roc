//! Tests verifying that "Builtin" is not in scope and cannot be imported,
//! but that nested types like Str, List, etc. are available.

const TestEnv = @import("./TestEnv.zig");
const testing = @import("std").testing;
const std = @import("std");

test "cannot import Builtin module" {
    const src =
        \\import Builtin
        \\
        \\x = 5
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    // Should have a canonicalization problem because Builtin is not a module that can be imported
    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.module_env.gpa.free(diagnostics);

    // Expect at least one diagnostic (module not found error)
    try testing.expect(diagnostics.len > 0);
}

test "can define userspace type named Builtin" {
    const src =
        \\Test := [A, B, C]
        \\
        \\Builtin := [D, E, F]
        \\
        \\x : Builtin
        \\x = D
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    // Should have no problems - Builtin is a valid userspace name
    try test_env.assertDefType("x", "Builtin");
}

test "builtin types are still available without import" {
    const src =
        \\Test := [Whatever]
        \\
        \\x : Str
        \\x = "hello"
        \\
        \\y : List(U64)
        \\y = [1, 2, 3]
    ;

    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    // Builtin types like Str and List should work without importing Builtin
    try test_env.assertDefType("x", "Str");
    try test_env.assertDefType("y", "List(Num(Int(Unsigned64)))");
}
