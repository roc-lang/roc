//! Tests that a user type declaration with a builtin type's name shadows that
//! builtin in every scope it is visible from, with one warning at the
//! declaration.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn expectOneShadowWarningAndNoTypeProblems(src: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", src);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expectEqual(std.meta.activeTag(diagnostics[0]), .builtin_type_shadowed_warning);
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
}

test "a top-level type named List shadows the builtin List in annotations" {
    try expectOneShadowWarningAndNoTypeProblems(
        \\List :: { item : U8 }.{
        \\    get : List -> U8
        \\    get = |list| list.item
        \\}
    );
}

test "a top-level generic type named Box shadows the builtin Box in annotations" {
    try expectOneShadowWarningAndNoTypeProblems(
        \\Box(a) :: { item : a }.{
        \\    get : Box(a) -> a
        \\    get = |box| box.item
        \\}
    );
}

test "a top-level type named U8 shadows the builtin U8 in annotations" {
    try expectOneShadowWarningAndNoTypeProblems(
        \\U8 :: { label : Str }.{
        \\    get : U8 -> Str
        \\    get = |u| u.label
        \\}
    );
}

test "a nested type named Str shadows the builtin Str inside its own block" {
    try expectOneShadowWarningAndNoTypeProblems(
        \\Outer :: [].{
        \\    Str :: { item : U8 }.{
        \\        get : Str -> U8
        \\        get = |s| s.item
        \\    }
        \\}
    );
}

test "a nested type named List shadows the builtin List for its siblings" {
    try expectOneShadowWarningAndNoTypeProblems(
        \\Outer :: [].{
        \\    List :: { item : U8 }
        \\
        \\    get : List -> U8
        \\    get = |list| list.item
        \\}
    );
}

test "a type nested two levels deep named Dict shadows the builtin Dict" {
    try expectOneShadowWarningAndNoTypeProblems(
        \\Outer :: [].{
        \\    Inner :: [].{
        \\        Dict :: { item : U8 }.{
        \\            get : Dict -> U8
        \\            get = |dict| dict.item
        \\        }
        \\    }
        \\}
    );
}

test "a type nested in the module's main type shadows the builtin at the top level" {
    var test_env = try TestEnv.init("Test",
        \\Test :: [].{
        \\    Str := { item : U8 }
        \\}
        \\
        \\get : Str -> U8
        \\get = |s| s.item
    );
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expectEqual(std.meta.activeTag(diagnostics[0]), .builtin_type_shadowed_warning);
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
    try test_env.assertDefTypeOptions("get", "Test.Str -> U8", .{ .allow_type_errors = false, .allow_can_errors = true });
}

test "builtin type names without a shadowing declaration still name the builtins" {
    var test_env = try TestEnv.init("Test",
        \\count : List(U8), Box(Str) -> U64
        \\count = |list, _box| list.len()
    );
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try test_env.assertDefType("count", "List(U8), Box(Str) -> U64");
}
