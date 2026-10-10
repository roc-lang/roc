//! Regression tests for tuple access on a control-flow receiver whose tuple
//! type still carries an open tag row, lowered at a sealed element type.

const harness = @import("lower_to_lir_harness.zig");

test "tuple access on a match result lowers as a compile-time equality operand" {
    try harness.expectLowersToLir(
        \\f : U64 -> Bool
        \\f = |_| (match (True, 1) { (_, _) => (False, 0) }).1 == 0
        \\
        \\main! = |_| {
        \\    echo!(Str.inspect(f(1)))
        \\    Ok({})
        \\}
    );
}

test "tuple access on an if result lowers as a compile-time comparison operand" {
    try harness.expectLowersToLir(
        \\f : U64 -> Bool
        \\f = |_| (if True (A, 0.U64) else (B, 1)).1 > 0
        \\
        \\main! = |_| {
        \\    echo!(Str.inspect(f(1)))
        \\    Ok({})
        \\}
    );
}
