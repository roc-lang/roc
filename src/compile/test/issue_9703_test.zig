//! Regression test for issue #9703.

const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLir = harness.expectLowersToLir;

test "issue 9703: recursive list effect with explicit root annotation lowers to LIR" {
    try expectLowersToLir(
        \\Model : { pins : List(U64) }
        \\
        \\f! = |a, _b, c|
        \\    match a {
        \\        [] => c
        \\        [_, .. as rest] => f!(rest, c, c)
        \\    }
        \\
        \\render! : Model, {} => Try(Model, _)
        \\render! = |model, _|
        \\    Ok({ pins: f!(model.pins, model.pins, model.pins) })
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    model : Model
        \\    model = { pins: [] }
        \\    _ = render!(model, {})
        \\    Ok({})
        \\}
    );
}
