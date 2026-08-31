//! End-to-end coverage for deterministic ordinary-specialization handoff.

const harness = @import("lower_to_lir_harness.zig");
const expectDeterministicLir = harness.expectDeterministicLir;

test "multiple ordinary specialization epochs lower deterministically" {
    try expectDeterministicLir(
        \\identity = |value| value
        \\
        \\through = |value| identity(value)
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    number = through(42)
        \\    text = through("epoch")
        \\    echo!(Str.inspect({ number, text }))
        \\    Ok({})
        \\}
    );
}
