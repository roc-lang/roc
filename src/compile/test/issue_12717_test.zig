//! Regression test for https://github.com/roc-lang/roc/issues/12717.
//!
//! `Rec` is an opaque record whose `step` closure built in `next` captures a
//! `Rec`, and whose `peek` closure has the same function type as `step`'s
//! argument. No type is recursive, so the app lowers to LIR cleanly. When run,
//! it prints `1`.
const harness = @import("lower_to_lir_harness.zig");

test "issue 12717: opaque type whose closures capture the same opaque type lowers to LIR" {
    try harness.expectLowersToLir(
        \\Rec :: { step : (U64 -> U64) -> U64, items : List(U64), peek : U64 -> U64 }.{
        \\    next = |rec| {
        \\        Rec.(r) = rec
        \\        Rec.({ step: |f| Rec.run(rec, f), items: [], peek: |_| r.items.len() })
        \\    }
        \\
        \\    run = |Rec.(r), f| r.items.append((r.step)(f)).len()
        \\}
        \\
        \\start = Rec.({ step: |_| 0, items: [], peek: |_| 0 })
        \\
        \\main! = |_args| {
        \\    echo!(start.next().run(|n| n).to_str())
        \\    Ok({})
        \\}
    );
}
