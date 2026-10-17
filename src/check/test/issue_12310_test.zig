//! A receiver whose same-name method one callee requires at one format and
//! another callee at a second format holds both requirements: each is an
//! independent use of the receiver's rank-1 method scheme.
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
    \\load = |{}| Json.parse("7")
    \\
;

test "issue 12310: an unannotated Json parser passes to a function requiring a second format" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\number : {} -> Try(U64, _)
        \\number = wrap(load)
    );
    defer env.deinit();
    try env.assertDefType("number", "{} -> Try(U64, [InvalidJson(Str)])");
}

test "issue 12310: an unannotated helper keeps both formats' parser requirements" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\both = |{}| wrap(load)({})
    );
    defer env.deinit();
    try env.assertDefType("both",
        \\{} -> Try(a, [InvalidJson(Str), ..errs])
        \\  where [
        \\    a.parser_for : Tiny -> (List(U8) -> Try({ rest: List(U8), value: a }, [Empty])),
        \\    a.parser_for : Encoding.JsonEncoding -> (Encoding.JsonState -> Try({ rest: Encoding.JsonState, value: a }, [InvalidJson(Str), ..errs])),
        \\  ]
    );
}

test "issue 12310: a generic helper requiring two formats is used at a type parsing both" {
    var env = try TestEnv.init("Test", tiny_format ++
        \\both = |{}| wrap(load)({})
        \\
        \\number : Try(U64, _)
        \\number = both({})
    );
    defer env.deinit();
    try env.assertDefType("number", "Try(U64, [InvalidJson(Str)])");
}
