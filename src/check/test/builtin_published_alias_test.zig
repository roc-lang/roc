//! Tests for builtin types named through an alias.
//!
//! `Str.Utf8Problem`, `Str.Utf16Problem` and `Str.Utf32Problem` are aliases of
//! nominal types declared below the Builtin module's block, private to it.
//! Roc code names only the aliases: types print under the alias's name,
//! members and tags resolve through either spelling of the alias, and the
//! nominal types' own names stay free for user code.

const TestEnv = @import("./TestEnv.zig");

test "a mismatch on a value built through Utf8Problem names Utf8Problem" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\x : U64
        \\x = Utf8Problem.InvalidStartByte
    );
    defer env.deinit();

    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\x = Utf8Problem.InvalidStartByte
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Utf8Problem
        \\
        \\But the annotation says it should be:
        \\
        \\    U64
        \\
        \\
    );
}

test "a mismatch on Str.from_utf8's result names Utf8Problem" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\x : U64
        \\x = Str.from_utf8([255])
    );
    defer env.deinit();

    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\x = Str.from_utf8([255])
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Try(Str, [BadUtf8({ index: U64, problem: Utf8Problem })])
        \\
        \\But the annotation says it should be:
        \\
        \\    U64
        \\
        \\
    );
}

test "the nominal types behind the Str problem aliases print under the alias names" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\eq8 : U8
        \\eq8 = Utf8Problem.is_eq
        \\
        \\eq16 : U16
        \\eq16 = Str.Utf16Problem.is_eq
        \\
        \\eq32 : U32
        \\eq32 = Utf32Problem.is_eq
    );
    defer env.deinit();

    try env.assertTypeErrorMsgs(&.{
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\eq8 = Utf8Problem.is_eq
        \\```
        \\      ^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Utf8Problem, Utf8Problem -> Bool
        \\
        \\But the annotation says it should be:
        \\
        \\    U8
        \\
        \\
        ,
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\eq16 = Str.Utf16Problem.is_eq
        \\```
        \\       ^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Utf16Problem, Utf16Problem -> Bool
        \\
        \\But the annotation says it should be:
        \\
        \\    U16
        \\
        \\
        ,
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\eq32 = Utf32Problem.is_eq
        \\```
        \\       ^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    Utf32Problem, Utf32Problem -> Bool
        \\
        \\But the annotation says it should be:
        \\
        \\    U32
        \\
        \\
    });
}

test "a type writer without an import mapping names the Str problem types by their aliases" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\StrUtf8Problem := [Mine]
        \\
        \\mine = StrUtf8Problem.Mine
        \\
        \\built = Utf8Problem.InvalidStartByte
        \\
        \\eq8 = Utf8Problem.is_eq
        \\
        \\eq16 = Str.Utf16Problem.is_eq
        \\
        \\eq32 = Utf32Problem.is_eq
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("mine", "StrUtf8Problem");
    try env.assertDefType("built", "Str.Utf8Problem");
    try env.assertDefType("eq8", "Str.Utf8Problem, Str.Utf8Problem -> Bool");
    try env.assertDefType("eq16", "Str.Utf16Problem, Str.Utf16Problem -> Bool");
    try env.assertDefType("eq32", "Str.Utf32Problem, Str.Utf32Problem -> Bool");
}

test "members resolve through both spellings of a Str problem alias" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\a = Utf8Problem.InvalidStartByte
        \\b = Str.Utf8Problem.OverlongEncoding
        \\
        \\short = Utf8Problem.is_eq(a, b)
        \\long = Str.Utf8Problem.is_eq(a, b)
        \\wide = Str.Utf16Problem.is_eq(Utf16Problem.UnpairedHighSurrogate, Str.Utf16Problem.UnpairedLowSurrogate)
        \\wider = Utf32Problem.is_eq(Str.Utf32Problem.SurrogateCodePoint, Utf32Problem.CodePointTooLarge)
        \\
        \\operator = a == b
        \\method = a.is_eq(b)
        \\
        \\name : Str.Utf8Problem -> Str
        \\name = |problem| match problem {
        \\    Utf8Problem.InvalidStartByte => "start"
        \\    Str.Utf8Problem.OverlongEncoding => "overlong"
        \\    _ => "other"
        \\}
        \\
        \\from_bytes = match Str.from_utf8([255]) {
        \\    Ok(_) => Bool.False
        \\    Err(BadUtf8({ problem, index: _ })) => problem == Utf8Problem.InvalidStartByte
        \\}
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("short", "Bool");
    try env.assertDefType("long", "Bool");
    try env.assertDefType("wide", "Bool");
    try env.assertDefType("wider", "Bool");
    try env.assertDefType("operator", "Bool");
    try env.assertDefType("method", "Bool");
    try env.assertDefType("from_bytes", "Bool");
}

test "a missing member of a Str problem alias is reported under the written name" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\x = Str.Utf8Problem.nope
    );
    defer env.deinit();

    try env.assertOneTypeErrorMsg(
        \\**Associated Constant Not Found**
        \\The type `Str.Utf8Problem` does not have an associated constant named `nope`.
        \\```roc
        \\x = Str.Utf8Problem.nope
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\
    );
}

test "a user type named like the nominal type behind a Str problem alias keeps its own name" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\StrUtf8Problem := [Mine]
        \\
        \\x : U64
        \\x = (StrUtf8Problem.Mine, Utf8Problem.InvalidStartByte, Utf8Problem.is_eq)
    );
    defer env.deinit();

    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This expression is used in an unexpected way.
        \\```roc
        \\x = (StrUtf8Problem.Mine, Utf8Problem.InvalidStartByte, Utf8Problem.is_eq)
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\It has the type:
        \\
        \\    (StrUtf8Problem, Utf8Problem, Utf8Problem, Utf8Problem -> Bool)
        \\
        \\But the annotation says it should be:
        \\
        \\    U64
        \\
        \\
    );
}

test "user code cannot name the nominal type behind a Str problem alias" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\x : StrUtf8Problem
        \\x = Utf8Problem.InvalidStartByte
    );
    defer env.deinit();

    try env.assertOneCanError("Undeclared Type");
}

test "the name of the nominal type behind a Str problem alias is not a reserved module name" {
    var env = try TestEnv.init("StrUtf8Problem",
        \\StrUtf8Problem := [Mine].{
        \\    mine : StrUtf8Problem
        \\    mine = StrUtf8Problem.Mine
        \\}
    );
    defer env.deinit();

    try env.assertNoErrors();
}
