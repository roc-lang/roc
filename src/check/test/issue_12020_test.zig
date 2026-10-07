//! Regression tests for issue 12020.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12020
//
// A type declared inside a function body is generated as a generalized
// template before value checking, like a top-level declaration. Every use of
// it, including an annotation that applies it to a type variable, instantiates
// that template rather than sharing the declaration's own formal variables.

test "issue 12020: associated method annotation applies a local generic nominal to its own variable" {
    var test_env = try TestEnv.init("Test",
        \\make_label = |value| {
        \\    Label(a) := { text : a }.{
        \\        render : Label(a) -> a
        \\        render = |label| label.text
        \\    }
        \\
        \\    Label.render(Label.{ text: value })
        \\}
    );
    defer test_env.deinit();
    try test_env.assertDefType("make_label", "a -> a");
}

test "issue 12020: local annotation applies a local generic nominal to a differently named variable" {
    var test_env = try TestEnv.init("Test",
        \\second = |left, right| {
        \\    Pair(a) := { first : a, second : a }
        \\
        \\    get : Pair(b) -> b
        \\    get = |pair| pair.second
        \\
        \\    get(Pair.{ first: left, second: right })
        \\}
    );
    defer test_env.deinit();
    try test_env.assertDefType("second", "b, b -> b");
}

test "issue 12020: local generic nominal annotation instantiated at two types" {
    var test_env = try TestEnv.init("Test",
        \\pair = |n, s| {
        \\    Label(a) := { text : a }
        \\
        \\    read : Label(b) -> b
        \\    read = |label| label.text
        \\
        \\    (read(Label.{ text: n }), read(Label.{ text: s }))
        \\}
        \\
        \\result = pair(1.I64, "x")
    );
    defer test_env.deinit();
    try test_env.assertDefType("result", "(I64, Str)");
}
