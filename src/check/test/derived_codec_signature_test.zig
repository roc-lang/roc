//! A derivable-method requirement whose signature no derived method has is
//! reported at the dispatch that needs it.

const TestEnv = @import("TestEnv.zig");

test "a derived encoder_for whose required signature is not an encoder is a missing method" {
    var env = try TestEnv.init("Test",
        \\stamp : r -> Str where [r.encoder_for : Str -> {}]
        \\stamp = |_| "ok"
        \\
        \\used = stamp({})
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}

test "a derived parser_for whose required signature is not a parser is a missing method" {
    var env = try TestEnv.init("Test",
        \\make : {} -> p where [p.parser_for : {} -> p]
        \\make = |_| {
        \\    P : p
        \\    P.parser_for({})
        \\}
        \\
        \\used : { a : U64 }
        \\used = make({})
    );
    defer env.deinit();
    try env.assertOneTypeError("Missing Method");
}
