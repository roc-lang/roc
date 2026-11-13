//! Tests for builtin declarations published through an alias.
//!
//! `Str.Utf8Problem`, `Str.Utf16Problem` and `Str.Utf32Problem` are aliases of
//! nominal types declared below the Builtin module's block, private to it. Roc code names only
//! the aliases: types print under the alias's name, members and tags resolve
//! through either spelling of the alias, and the declarations' own names stay
//! free for user code.

const std = @import("std");
const CIR = @import("can").CIR;
const compiled_builtins = @import("compiled_builtins");
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

    try env.assertOneCanErrorMsg(
        \\**Does Not Exist**
        \\`Str.Utf8Problem.nope` does not exist.
        \\`Str.Utf8Problem` is in scope, but it has no associated `nope`.
        \\
        \\```roc
        \\x = Str.Utf8Problem.nope
        \\```
        \\    ^^^^^^^^^^^^^^^^^^^^
        \\
        \\
        \\
    );
}

test "a user type named like a published builtin declaration keeps its own name" {
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

test "user code cannot name a published builtin declaration" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\x : StrUtf8Problem
        \\x = Utf8Problem.InvalidStartByte
    );
    defer env.deinit();

    try env.assertOneCanError("Undeclared Type");
}

test "a published builtin declaration's name is not a reserved module name" {
    var env = try TestEnv.init("StrUtf8Problem",
        \\StrUtf8Problem := [Mine].{
        \\    mine : StrUtf8Problem
        \\    mine = StrUtf8Problem.Mine
        \\}
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "the registry names every builtin type the way its declaration does" {
    var env = try TestEnv.init("Repro", "Repro :: [].{}");
    defer env.deinit();

    const builtin_env = env.builtin_module.env;
    const indices = compiled_builtins.builtinIndices(CIR);
    inline for (CIR.builtin_type_specs) |spec| {
        const header_idx = switch (builtin_env.store.getStatement(@field(indices, spec.type_field))) {
            .s_nominal_decl => |decl| decl.header,
            .s_alias_decl => |decl| decl.header,
            .s_decl,
            .s_var,
            .s_var_uninitialized,
            .s_reassign,
            .s_crash,
            .s_dbg,
            .s_expr,
            .s_expect,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_break,
            .s_return,
            .s_import,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => return error.TestUnexpectedResult,
        };
        const header = builtin_env.store.getTypeHeader(header_idx);
        try std.testing.expectEqualStrings(spec.qualified_name, builtin_env.getIdentText(header.name));
        try std.testing.expectEqualStrings(spec.qualified_name, builtin_env.getIdentText(header.relative_name));
        // A type declared inside the Builtin block carries the module's
        // qualification, which no user type can have. One published through
        // an alias is declared below the block under a bare name, and is
        // recognized by its origin instead.
        try std.testing.expectEqual(
            spec.published_as == null,
            std.mem.startsWith(u8, spec.qualified_name, CIR.builtin_module_prefix),
        );
        // Only a declaration published through an alias is shown under a
        // name other than the one it is declared with.
        try std.testing.expectEqual(
            spec.published_as == null,
            std.mem.eql(u8, spec.display_name, CIR.builtinTypeDeclaredName(spec)),
        );
    }

    try std.testing.expectEqual(@as(usize, 3), indices.alias_targets.len);
    for (indices.alias_targets) |target| {
        const alias_header = builtin_env.store.getTypeHeader(builtin_env.store.getStatement(target.alias).s_alias_decl.header);
        const nominal_header = builtin_env.store.getTypeHeader(builtin_env.store.getStatement(target.nominal).s_nominal_decl.header);
        const published = CIR.builtinTypePublishedName(builtin_env.getIdentText(nominal_header.name)) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqualStrings(published.alias_qualified_name, builtin_env.getIdentText(alias_header.name));
    }
}
