//! A generated-codec relation that reaches a definition's quantified variables
//! is a template its uses copy: finalization validates it on a private
//! instance, so the definition's scheme is exactly what its uses instantiated,
//! and every use owns its own state and error row (design.md "Parked Codec
//! Relations Validate A Private Instance").
const TestEnv = @import("TestEnv.zig");

const prelude =
    \\
    \\Format := [Default].{
    \\    parse_u64 : Format, U64 -> Try({ value : U64, rest : U64 }, [Negative])
    \\    parse_u64 = |_, _state| Err(Negative)
    \\
    \\    encode_u64 : U64, U64 -> Try(U64, [Overflow])
    \\    encode_u64 = |value, state| Ok(state + value)
    \\}
    \\
    \\Wide : [Negative, Reset(Str)]
    \\
    \\any_parser = |_| U64.parser_for(Format.Default)
    \\
;

test "an unannotated parser definition keeps its state and error row quantified" {
    var env = try TestEnv.init("Test", prelude);
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("any_parser", "_arg -> (a -> Try({ rest: a, value: U64 }, _b))");
}

test "an unannotated encoder definition keeps its state and error row quantified" {
    var env = try TestEnv.init("Test", prelude ++
        \\any_encoder = |_| U64.encoder_for(Format.Default)
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("any_encoder", "_arg -> (U64, a -> Try(a, _b))");
}

test "a definition forwarding a parser keeps the forwarded relation quantified" {
    var env = try TestEnv.init("Test", prelude ++
        \\forwarding = |x| any_parser(x)
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("forwarding", "_arg -> (a -> Try({ rest: a, value: U64 }, _b))");
}

test "two calls in one body take one parser at two error rows" {
    var env = try TestEnv.init("Test", prelude ++
        \\both = |{}| {
        \\    narrow : U64 -> Try({ value : U64, rest : U64 }, [Negative])
        \\    narrow = any_parser({})
        \\    wide : U64 -> Try({ value : U64, rest : U64 }, Wide)
        \\    wide = any_parser({})
        \\    (narrow, wide)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "each use validates its own state against the format" {
    var env = try TestEnv.init("Test", prelude ++
        \\parse_text : Str -> Try({ value : U64, rest : Str }, [Negative])
        \\parse_text = any_parser({})
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "each use validates its own error row against the format" {
    var env = try TestEnv.init("Test", prelude ++
        \\other = |{}| {
        \\    parse : U64 -> Try({ value : U64, rest : U64 }, [Other])
        \\    parse = any_parser({})
        \\    match parse(7) {
        \\        Ok(_) => "ok"
        \\        Err(Other) => "other"
        \\    }
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Row Missing Tag");
}

test "a relation no type can satisfy is reported without any use" {
    var env = try TestEnv.init("Test",
        \\
        \\Empty := [Default].{}
        \\
        \\any_parser = |_| U64.parser_for(Empty.Default)
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "a body cannot deliver an annotation's rigid state" {
    var env = try TestEnv.init("Test", prelude ++
        \\promised : {} -> (s -> Try({ value : U64, rest : s }, [Negative]))
        \\promised = |_| U64.parser_for(Format.Default)
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "a body cannot deliver an annotation's rigid error row" {
    var env = try TestEnv.init("Test", prelude ++
        \\promised : {} -> (U64 -> Try({ value : U64, rest : U64 }, e))
        \\promised = |_| U64.parser_for(Format.Default)
    );
    defer env.deinit();
    try env.assertOneTypeError("Parser Error Row Missing Tag");
}

test "a relation reaching a weak value still determines that value" {
    var env = try TestEnv.init("Test", prelude ++
        \\seeds = []
        \\
        \\first : List(item) -> item
        \\first = |_| crash "first"
        \\
        \\parse_seed = |_| U64.parser_for(Format.Default)(first(seeds))
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("seeds", "List(U64)");
}

test "a relation whose state is known validates at its boundary" {
    var env = try TestEnv.init("Test", prelude ++
        \\known = |state| {
        \\    typed : U64
        \\    typed = state
        \\    U64.parser_for(Format.Default)(typed)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("known", "U64 -> Try({ rest: U64, value: U64 }, [Negative])");
}

test "a local nothing calls still relates the enclosing variable it captures" {
    var env = try TestEnv.init("Test", prelude ++
        \\unused_local = |state| {
        \\    _local = |_| U64.parser_for(Format.Default)(state)
        \\    1
        \\}
        \\
        \\at_text = unused_local("text")
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "a local read but never called still relates the enclosing variable it captures" {
    var env = try TestEnv.init("Test", prelude ++
        \\read_local = |state| {
        \\    local = |_| U64.parser_for(Format.Default)(state)
        \\    _ = local
        \\    1
        \\}
        \\
        \\at_text = read_local("text")
    );
    defer env.deinit();
    try env.assertOneTypeError("Type Mismatch");
}

test "a local nothing calls accepts the enclosing variable at the format's state" {
    var env = try TestEnv.init("Test", prelude ++
        \\unused_local = |state| {
        \\    _local = |_| U64.parser_for(Format.Default)(state)
        \\    1
        \\}
        \\
        \\at_state = |{}| {
        \\    state : U64
        \\    state = 3
        \\    unused_local(state)
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}
