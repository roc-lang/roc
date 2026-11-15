//! Regression coverage for https://github.com/roc-lang/roc/issues/13040.
//! A recursive local closure captures a runtime value and defines a capturing
//! closure that it passes to `List.update` both in a `match` scrutinee and in
//! one of the `match` branches. The program type-checks and must lower to LIR.

const harness = @import("lower_to_lir_harness.zig");

test "issue 13040: capturing closure passed to List.update twice inside a recursive local closure lowers" {
    try harness.expectLowersToLir(
        \\run : List(U8), U64 -> List(U8)
        \\run = |list, bp| {
        \\    loops = |s, cs| {
        \\        if (s >= 100) cs else {
        \\            smsk = s.to_u8_wrap()
        \\            updtf = |a| a.bitwise_or(smsk)
        \\            match List.update(cs, s, updtf) {
        \\                Err(_) => loops(s + bp, cs)
        \\                Ok(first) => loops(s + bp, List.update(first, s + 1, updtf).ok_or([]))
        \\            }
        \\        }
        \\    }
        \\    loops(0, list)
        \\}
        \\
        \\main! = |args| {
        \\    out = run(List.repeat(0.U8, 16), List.len(args) + 3)
        \\    echo!(out.len().to_str())
        \\    Ok({})
        \\}
    );
}
