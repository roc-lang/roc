//! A type has one method of each name. Requirements of one method on a type
//! that is still unknown are each checked against the type it resolves to;
//! when the type is generalized instead, they become its one requirement of
//! that method, and requirements that cannot be one are a type mismatch whose
//! report shows both required method types.
const TestEnv = @import("TestEnv.zig");

const tiny_format =
    \\Tiny := [Tiny].{
    \\    a.Parseable(errs) :
    \\        where [
    \\            a.parser_for : Tiny -> (List(U8) -> Try({ value : a, rest : List(U8) }, errs)),
    \\        ]
    \\
    \\    parse_u64 : Tiny, List(U8) -> Try({ value : U64, rest : List(U8) }, [Empty])
    \\    parse_u64 = |_, bytes| match bytes {
    \\        [b, .. as rest] => Ok({ value: b.to_u64(), rest })
    \\        [] => Err(Empty)
    \\    }
    \\}
    \\
    \\wrap : ({} -> Try(a, e)) -> ({} -> Try(a, e)) where [a.Tiny.Parseable([Empty])]
    \\wrap = |f| f
    \\
;

test "issue 12310: a Json parser passed where a second format's parser is required checks against the type it resolves to" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\load = |{}| Json.parse("7")
        \\
        \\number : {} -> Try(U64, _)
        \\number = wrap(load)
    );
    defer env.deinit();
    try env.assertDefType("number", "{} -> Try(U64, [InvalidJson(Str)])");
}

test "issue 12310: a generalized type requiring two formats' parser_for is one method conflict" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\both = |{}| wrap(|{}| Json.parse("7"))({})
    );
    defer env.deinit();
    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `parser_for` methods, but no type can have more than one `parser_for` method.
        \\```roc
        \\both = |{}| wrap(|{}| Json.parse("7"))({})
        \\```
        \\                      ^^^^^^^^^^
        \\
        \\One part of this use needs `parser_for` to have this type:
        \\
        \\    Tiny -> (List(U8) -> Try({ rest: List(U8), value: a }, [Empty]))
        \\
        \\Another part needs it to have this type:
        \\
        \\    JsonEncoding -> (JsonState -> Try({ rest: JsonState, value: a }, [InvalidJson(Str)]))
        \\
        \\A type has only one `parser_for` method, so no type can satisfy both.
        \\
        \\
    );
}

test "issue 12310: dot calls of one method on a generalized type are one requirement" {
    var env = try TestEnv.init("Test",
        \\pair = |x, y, z| (x.combine(y), x.combine(z))
    );
    defer env.deinit();
    try env.assertDefType("pair", "a, b, b -> (c, c) where [a.combine : a, b -> c]");
}

test "issue 12310: dot calls of one method at incompatible types on a generalized type are one method conflict" {
    var env = try TestEnv.init("Test",
        \\pair = |x, text| (x.combine(1.U64), x.combine(Str.trim(text)))
    );
    defer env.deinit();
    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `combine` methods, but no type can have more than one `combine` method.
        \\```roc
        \\pair = |x, text| (x.combine(1.U64), x.combine(Str.trim(text)))
        \\```
        \\                  ^^^^^^^^^^^^^^^^
        \\
        \\One part of this use needs `combine` to have this type:
        \\
        \\    a, Str -> _ret
        \\
        \\Another part needs it to have this type:
        \\
        \\    a, U64 -> _ret
        \\
        \\A type has only one `combine` method, so no type can satisfy both.
        \\
        \\
    );
}

test "issue 12310: an annotation requiring two formats' parser_for is one method conflict" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\both : {} -> Try(a, [InvalidJson(Str), ..errs])
        \\    where [a.Tiny.Parseable([Empty]), a.Json.Parseable([InvalidJson(Str), ..errs])]
        \\both = |{}| crash "unused"
    );
    defer env.deinit();
    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `parser_for` methods, but no type can have more than one `parser_for` method.
        \\```roc
        \\    where [a.Tiny.Parseable([Empty]), a.Json.Parseable([InvalidJson(Str), ..errs])]
        \\```
        \\                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \\
        \\One part of this use needs `parser_for` to have this type:
        \\
        \\    Tiny -> (List(U8) -> Try({ rest: List(U8), value: _field }, [Empty]))
        \\
        \\Another part needs it to have this type:
        \\
        \\    JsonEncoding -> (JsonState -> Try({ rest: JsonState, value: _field }, [InvalidJson(Str)]))
        \\
        \\A type has only one `parser_for` method, so no type can satisfy both.
        \\
        \\
    );
}

test "issue 12310: a parser whose type is already known meets the second format's requirement" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\load : {} -> Try(U64, [InvalidJson(Str)])
        \\load = |{}| Json.parse("7")
        \\
        \\number : {} -> Try(U64, _)
        \\number = wrap(load)
    );
    defer env.deinit();
    try env.assertNoErrors();
}
