//! End-to-end coverage for deterministic ordinary-specialization handoff.

const harness = @import("lower_to_lir_harness.zig");
const expectSpecializationParallelismDeterministicLir = harness.expectSpecializationParallelismDeterministicLir;
const expectProcedureRootParallelismDeterministicLir = harness.expectProcedureRootParallelismDeterministicLir;

test "multiple procedure-use roots lower deterministically in parallel" {
    try expectProcedureRootParallelismDeterministicLir(
        \\main! = |_args| Ok({})
    );
}

test "multiple ordinary specialization epochs lower deterministically in parallel" {
    // `through` has independent scalar, list, and record specializations. Each
    // discovers its generic call to `identity`, exercising a second wave plus
    // worker-local type spans and field names while worker schedules vary.
    try expectSpecializationParallelismDeterministicLir(
        \\identity = |value| value
        \\
        \\through = |value| identity(value)
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    number = through(42)
        \\    text = through("epoch")
        \\    items = through([1, 2])
        \\    record = through({ label: "parallel" })
        \\    echo!(Str.inspect({ number, text, items, record }))
        \\    Ok({})
        \\}
    );
}
