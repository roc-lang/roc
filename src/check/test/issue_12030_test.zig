//! Regression tests for issue 12030.

const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12030
//
// Using a value where a `Str` is needed reports a type mismatch whose hint says
// how to turn the value into a `Str`: `to_str` when its type has that method,
// defining `to_str` for a nominal type declared in the module being checked,
// and `Str.inspect` for a debugging representation otherwise.

test "issue 12030: number literal passed where Str is needed suggests to_str" {
    const source =
        \\echo : Str -> {}
        \\echo = |_| {}
        \\
        \\main = echo(1)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "to_str");
}

test "issue 12030: typed number passed where Str is needed suggests to_str" {
    const source =
        \\echo : Str -> {}
        \\echo = |_| {}
        \\
        \\n : U64
        \\n = 1
        \\
        \\main = echo(n)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "to_str");
}

test "issue 12030: nominal type declared here without to_str suggests defining one" {
    const source =
        \\echo : Str -> {}
        \\echo = |_| {}
        \\
        \\Color := [Red, Green]
        \\
        \\red : Color
        \\red = Color.Red
        \\
        \\main = echo(red)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\The first argument being passed to this function has the wrong type.
        \\```roc
        \\main = echo(red)
        \\```
        \\            ^^^
        \\
        \\This argument has the type:
        \\
        \\    Color
        \\
        \\But `echo` needs the first argument to be:
        \\
        \\    Str
        \\
        \\**Hint:** This type has no `to_str` method. Give it one to convert its values to a `Str`, or use `Str.inspect` to get a representation of a value for debugging.
        \\
        \\
    );
}

test "issue 12030: record passed where Str is needed suggests Str.inspect" {
    const source =
        \\echo : Str -> {}
        \\echo = |_| {}
        \\
        \\main = echo({ done: Bool.True })
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\The first argument being passed to this function has the wrong type.
        \\```roc
        \\main = echo({ done: Bool.True })
        \\```
        \\            ^^^^^^^^^^^^^^^^^^^
        \\
        \\This argument has the type:
        \\
        \\    { done: Bool }
        \\
        \\But `echo` needs the first argument to be:
        \\
        \\    Str
        \\
        \\**Hint:** To get a representation of this value for debugging, use `Str.inspect`.
        \\
        \\
    );
}

test "issue 12030: Str passed where a number is needed has no conversion hint" {
    const source =
        \\takes_num : U64 -> {}
        \\takes_num = |_| {}
        \\
        \\main = takes_num("one")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This string literal is being used where a non-string type is needed.
        \\```roc
        \\main = takes_num("one")
        \\```
        \\                 ^^^^^
        \\
        \\The type was determined to be:
        \\
        \\    U64
        \\
        \\
    );
}

test "issue 12030: function passed where Str is needed has no conversion hint" {
    const source =
        \\echo : Str -> {}
        \\echo = |_| {}
        \\
        \\main = echo(|x| x)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\The first argument being passed to this function has the wrong type.
        \\```roc
        \\main = echo(|x| x)
        \\```
        \\            ^^^
        \\
        \\This argument has the type:
        \\
        \\    a -> a
        \\
        \\But `echo` needs the first argument to be:
        \\
        \\    Str
        \\
        \\
    );
}

test "issue 12030: number literal hint offers quotes and to_str" {
    const source =
        \\echo : Str -> {}
        \\echo = |_| {}
        \\
        \\main = echo(1)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This number is being used where a non-number type is needed.
        \\```roc
        \\main = echo(1)
        \\```
        \\            ^
        \\
        \\Other code expects this to have the type:
        \\
        \\    Str
        \\
        \\**Hint:** Write the number in quotes to make it a `Str`, or convert it with `.to_str()`.
        \\
        \\
    );
}

test "issue 12030: typed number interpolated into a Str suggests to_str" {
    const source =
        \\n : U64
        \\n = 5
        \\
        \\main = "count: ${n}"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "`.to_str()`");
}

test "issue 12030: number literal value interpolated into a Str suggests to_str" {
    const source =
        \\main = {
        \\    n = 5
        \\    "count: ${n}"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeProblemMsgContains("Type Mismatch", "`.to_str()`");
}

test "issue 12030: record under a Str annotation has no conversion hint" {
    const source =
        \\label : Str
        \\label = { done: Bool.True }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\label = { done: Bool.True }
        \\```
        \\        ^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    { done: Bool }
        \\
        \\But the annotation says it should be:
        \\
        \\    Str
        \\
        \\
    );
}
