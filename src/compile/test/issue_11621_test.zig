//! Regression tests for https://github.com/roc-lang/roc/issues/11621.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 11621: mutually recursive functions using ? lower to LIR" {
    // Each function's error row includes the other's, so instantiating their
    // schemes copies rows that repeat a tag. Every published type root, not
    // only the rows reachable from expression types, must be normalized
    // before the checked module is built.
    try expectLowersToLir(
        \\step : U64 -> Try([More, Done], [StepFailed])
        \\step = |n| if n == 7 { Err(StepFailed) } else if n < 3 { Ok(More) } else { Ok(Done) }
        \\
        \\stop : U64 -> Try([More, Done], [StopFailed])
        \\stop = |n| if n == 9 { Err(StopFailed) } else if n < 3 { Ok(More) } else { Ok(Done) }
        \\
        \\ping = |n, out|
        \\    match step(n)? {
        \\        Done => pong(n + 1, out)
        \\        More => pong(n + 1, out.append(n))
        \\    }
        \\
        \\pong = |n, out|
        \\    match stop(n)? {
        \\        Done => Ok(out)
        \\        More => ping(n + 1, out.append(n))
        \\    }
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    _ = ping(0, [])
        \\    Ok({})
        \\}
    );
}
