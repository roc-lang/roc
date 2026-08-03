//! Regression test for issue #10508.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10508: unannotated named tag wrapper passed to List.map lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10508.
    // Passing an unannotated named function directly to List.map must lower.
    try expectLowersToLir(
        \\wrap = |x| Wrapped(x)
        \\
        \\main! = |args| {
        \\    tags = args.map(wrap)
        \\    match tags.len() {
        \\        0 => Ok({})
        \\        _ => Ok({})
        \\    }
        \\}
    );
}
