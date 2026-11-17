//! Regression for https://github.com/roc-lang/roc/issues/13106.
//! Import bindings cannot shadow builtin types. Rejected bindings produce
//! one declaration-site error, not misleading errors at their uses.

const std = @import("std");
const CIR = @import("can").CIR;
const TestEnv = @import("./TestEnv.zig");

const foo_source =
    \\Foo := [].{
    \\    Bool := [Yes, No].{
    \\        show : Bool -> Str
    \\        show = |b| match b {
    \\            Yes => "yes"
    \\            No => "no"
    \\        }
    \\    }
    \\}
;

fn expectDiagnostic(env: *TestEnv, expected: std.meta.Tag(CIR.Diagnostic)) (std.mem.Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    try std.testing.expect(!env.parse_ast.hasErrors());
    const diagnostics = try env.module_env.getDiagnostics();
    defer env.gpa.free(diagnostics);

    for (diagnostics) |diagnostic| {
        if (std.meta.activeTag(diagnostic) != expected) {
            std.debug.print("unexpected canonicalization diagnostic: {s}\n", .{@tagName(diagnostic)});
        }
    }
    for (env.checker.problems.problems.items) |problem| {
        std.debug.print("unexpected type problem: {s}\n", .{@tagName(problem)});
    }

    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expectEqual(expected, std.meta.activeTag(diagnostics[0]));
    try std.testing.expectEqual(@as(usize, 0), env.checker.problems.problems.items.len);
}

test "issue 13106: reject exposed Bool without associated lookup cascades" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    var env = try TestEnv.initWithImport("Main",
        \\import Foo exposing [Bool]
        \\
        \\Main := []
        \\
        \\show : Bool -> Str
        \\show = Bool.show
    , "Foo", &foo);
    defer env.deinit();

    try expectDiagnostic(&env, .builtin_import_conflict);
}

test "issue 13106: reject exposed Bool without nominal tag cascades" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    var env = try TestEnv.initWithImport("Main",
        \\import Foo exposing [Bool]
        \\
        \\Main := []
        \\
        \\value : Bool
        \\value = Bool.Yes
    , "Foo", &foo);
    defer env.deinit();

    try expectDiagnostic(&env, .builtin_import_conflict);
}

test "issue 13106: reject import aliases including forward uses and patterns" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    inline for (.{
        "import Foo exposing [Bool]",
        "import Foo exposing [Bool as Bool]",
        "import Foo exposing [Bool.*]",
        "import Foo.Bool",
        "import Foo as Bool",
    }) |import_source| {
        var env = try TestEnv.initWithImport("Main",
            \\Main := []
            \\show : Bool -> Str
            \\show = |b| match b {
            \\    Bool.Yes => Bool.show(b)
            \\    Bool.No => "no"
            \\}
            \\value = Bool.Yes
            \\
        ++ import_source, "Foo", &foo);
        defer env.deinit();
        try expectDiagnostic(&env, .builtin_import_conflict);
    }
}

test "issue 13106: exposing under a nonconflicting name remains valid" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    var env = try TestEnv.initWithImport("Main",
        \\import Foo exposing [Bool as Flag]
        \\Main := []
        \\value : Flag
        \\value = Flag.Yes
        \\text : Str
        \\text = Flag.show(value)
    , "Foo", &foo);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13106: an exposed alias cannot claim a different builtin name" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    var env = try TestEnv.initWithImport("Main",
        \\import Foo exposing [Bool as Str, Bool as Flag]
        \\Main := []
        \\bad : Str
        \\bad = Str.Yes
        \\good : Flag
        \\good = Flag.Yes
        \\negated = !True
    , "Foo", &foo);
    defer env.deinit();
    try expectDiagnostic(&env, .builtin_import_conflict);
    const diagnostics = try env.module_env.getDiagnostics();
    defer env.gpa.free(diagnostics);
    try std.testing.expectEqualStrings("Str", env.module_env.getIdent(diagnostics[0].builtin_import_conflict.name));
}

test "issue 13106: rejected Bool does not change compiler-owned negation" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    var env = try TestEnv.initWithImport("Main",
        \\import Foo exposing [Bool]
        \\Main := []
        \\bad = Bool.Yes
        \\negated = !True
    , "Foo", &foo);
    defer env.deinit();
    try expectDiagnostic(&env, .builtin_import_conflict);
    try env.assertDefTypeOptions("negated", "Bool", .{ .allow_type_errors = false, .allow_can_errors = true });
}

test "issue 13106: rejected builtin aliases retire literal suffixes and backing patterns" {
    var foo = try TestEnv.init("Foo", foo_source);
    defer foo.deinit();
    try expectDiagnostic(&foo, .builtin_type_shadowed_warning);

    var env = try TestEnv.initWithImport("Main",
        \\import Foo exposing [Bool as U8]
        \\Main := []
        \\value = 1.U8
        \\constructed = U8.(Yes)
        \\unwrap = |U8.(x)| x
    , "Foo", &foo);
    defer env.deinit();
    try expectDiagnostic(&env, .builtin_import_conflict);
}
