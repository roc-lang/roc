//! A type has one method of each name, so a use that requires one type to have
//! two different methods of the same name is a type mismatch. The report says
//! so and shows both required method types, and the conflicting type reports
//! nothing further.
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

test "issue 12310: a Json parser passed where a second format's parser is required is one method conflict" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\load = |{}| Json.parse("7")
        \\
        \\number : {} -> Try(U64, _)
        \\number = wrap(load)
    );
    defer env.deinit();
    try env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\This is used in a way that requires its type to have two different, incompatible `parser_for` methods, but no type can have more than one `parser_for` method.
        \\```roc
        \\number = wrap(load)
        \\```
        \\              ^^^^
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
