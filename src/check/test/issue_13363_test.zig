//! Regression tests for https://github.com/roc-lang/roc/issues/13363.
//! A nominal declaration body and a host-boundary annotation reference a
//! declaration at the same position, so they read one shared ground instance.
//! That instance is a monotype wherever it is read.

const TestEnv = @import("TestEnv.zig");

fn expectConcreteProvidedFunction(comptime declarations: []const u8, comptime expected_type: []const u8) TestEnv.TestEnvError!void {
    var env = try TestEnv.init("Platform",
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_shape": shape }
        \\
    ++ "\n" ++ declarations);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("shape", expected_type);
}

test "issue 13363: a nominal wrapping an imported type leaves a provided function concrete" {
    try expectConcreteProvidedFunction(
        \\Row := { value : Str }
        \\
        \\shape : List(Str) -> List(Str)
        \\shape = |value| value
    , "List(Str) -> List(Str)");
}

test "issue 13363: a nominal wrapping a local nominal leaves a provided function concrete" {
    try expectConcreteProvidedFunction(
        \\Handle := { reference : Str }
        \\
        \\Row := { value : Handle }
        \\
        \\shape : List(Handle) -> List(Handle)
        \\shape = |value| value
    , "List(Handle) -> List(Handle)");
}

test "issue 13363: a nominal wrapping a local alias leaves a provided function concrete" {
    try expectConcreteProvidedFunction(
        \\Inner := { reference : Str }
        \\
        \\Handle : Inner
        \\
        \\Row := { value : Handle }
        \\
        \\shape : List(Handle) -> List(Handle)
        \\shape = |value| value
    , "List(Handle) -> List(Handle)");
}

test "issue 13363: a nominal wrapping an applied type leaves a provided function concrete" {
    try expectConcreteProvidedFunction(
        \\Row := { values : List(Str) }
        \\
        \\shape : {} -> { values : List(Str) }
        \\shape = |_| { values: [] }
    , "{} -> { values: List(Str) }");
}
