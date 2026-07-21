//! Regression test for issue #10271.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10271: nested effectful try mapping lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10271.
    // Nested effectful `?` calls with an error mapper must preserve the
    // concrete callable's dispatcher paths through post-check lowering.
    try expectLowersToLir(
        \\step! : () => Try({}, [StepErr(Str), ..])
        \\step! = || Ok({})
        \\
        \\run! = |task| {
        \\    _ = step!()?
        \\    Ok(task()?)
        \\}
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    result = run!(
        \\        || {
        \\            _ = step!()?
        \\            Err(BadInput("oops"))
        \\        },
        \\    ) ? |err| RunFailed(err)
        \\    _ = result
        \\    Ok({})
        \\}
    );
}
