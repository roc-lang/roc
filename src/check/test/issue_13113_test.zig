//! Regression tests for https://github.com/roc-lang/roc/issues/13113.
//! A for-clause binds the app-facing variable separately from the platform's
//! abstract alias. Host entrypoints must not quantify fresh type variables.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

fn platformSource(comptime required_model: []const u8, comptime release_annotation: []const u8) []const u8 {
    return (
        \\platform ""
        \\    requires { [Model : model] for program : { init : {} -> Box(
    ) ++ required_model ++ (
        \\) } }
        \\    exposes []
        \\    packages {}
        \\    provides {
        \\        "roc_init": init_for_host,
        \\        "roc_release": release_for_host,
        \\    }
        \\
        \\init_for_host = program.init
        \\
    ) ++ "\n" ++ release_annotation ++ "\n" ++
        \\release_for_host = |_model| {}
    ;
}

test "issue 13113: requires accepts its bound type variable" {
    var env = try TestEnv.init("Platform", platformSource("model", "release_for_host : Box(Model) -> {}"));
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13113: requires rejects its for-clause alias" {
    var env = try TestEnv.init("Platform", platformSource("Model", "release_for_host : Box(Model) -> {}"));
    defer env.deinit();

    // This is a name-scope error, not an app/platform unification failure.
    // Pin rejection without prescribing the new diagnostic's wording.
    const diagnostics = try env.module_env.getDiagnostics();
    defer env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    var report = try env.module_env.diagnosticToReport(diagnostics[0], env.gpa, env.module_env.module_name);
    defer report.deinit();
    try std.testing.expect(report.severity.isError());
}

test "issue 13113: provides accepts the platform abstract alias" {
    var env = try TestEnv.init("Platform", platformSource("model", "release_for_host : Box(Model) -> {}"));
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("release_for_host", "Box(Model) -> {}");
}

fn expectRejectedProvidedType(comptime annotation: []const u8) TestEnv.TestEnvError!void {
    var env = try TestEnv.init("Platform", platformSource("model", annotation));
    defer env.deinit();

    // Lowercase names here are legal ordinary type variables, but a host
    // cannot instantiate a polymorphic entrypoint. Require a checking error.
    const diagnostics = try env.module_env.getDiagnostics();
    defer env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try std.testing.expectEqual(@as(usize, 1), try env.typeProblemCount());
}

test "issue 13113: provides rejects Box(model)" {
    try expectRejectedProvidedType("release_for_host : Box(model) -> {}");
}

test "issue 13113: provides rejects a bare model variable" {
    try expectRejectedProvidedType("release_for_host : model -> {}");
}

test "issue 13113: provides rejects an unrelated type variable" {
    try expectRejectedProvidedType("release_for_host : Box(a) -> {}");
}

test "issue 13113: provides rejects inferred polymorphism" {
    try expectRejectedProvidedType("");
}

test "issue 13113: provided diagnostic suggests the abstract alias and retires the definition" {
    var env = try TestEnv.init("Platform", platformSource("model", "release_for_host : Box(model) -> {}"));
    defer env.deinit();
    try env.assertOneTypeProblemMsgContains("Polymorphic Provided Function", "Use `Model` instead of `model`");
    const expr = try env.defExpr("release_for_host");
    try std.testing.expect(env.module_env.store.getExpr(expr) == .e_runtime_error);
}

test "issue 13113: provides checks inferred forwarding and polymorphism under aliases" {
    const declarations = [_][]const u8{
        \\release = |_value| {}
        \\entry = release
        ,
        \\Handle(a) : Box(a)
        \\entry : Handle(model) -> {}
        \\entry = |_value| {}
        ,
        \\entry : { value : Box(a) } -> {}
        \\entry = |_value| {}
        ,
        \\entry : {} -> List(a)
        \\entry = |_| []
        ,
    };
    inline for (declarations) |declaration| {
        var env = try TestEnv.init("Platform",
            \\platform ""
            \\    requires {}
            \\    exposes []
            \\    packages {}
            \\    provides { "roc_entry": entry }
            \\
        ++ "\n" ++ declaration);
        defer env.deinit();
        try env.assertOneTypeError("Polymorphic Provided Function");
    }
}

test "issue 13113: ordinary polymorphic helpers and monomorphic forwarded entrypoints remain valid" {
    var env = try TestEnv.init("Platform",
        \\platform ""
        \\    requires { [Model : model] for program : { init : {} -> Box(model) } }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_release": release_for_host, "roc_init": init_for_host }
        \\
        \\release : Box(a) -> {}
        \\release = |_value| {}
        \\Handle : Box(Model)
        \\release_for_host : Handle -> {}
        \\release_for_host = release
        \\init_for_host = program.init
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 13113: a subsequent requirement may use the established abstract alias" {
    var env = try TestEnv.init("Platform",
        \\platform ""
        \\    requires {
        \\        [Model : model] for initial : model,
        \\        update : Model -> Model,
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_entry": entry }
        \\
        \\entry : {} -> Box(Model)
        \\entry = |_| Box.box(update(initial))
    );
    defer env.deinit();
    try env.assertNoErrors();
}
