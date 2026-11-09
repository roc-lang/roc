//! Regression test for https://github.com/roc-lang/roc/issues/12752:
//! a `dbg` whose operand is a dispatch the checker rejected (a method call
//! on the result of a method call with the wrong number of arguments,
//! reached through an unannotated helper) reports its diagnostic and lowers
//! to LIR without a Monotype compiler panic. The `dbg` statement sits in a
//! compile-time constant, whose body lowers the rejected dispatch's static plan.

const harness = @import("lower_to_lir_harness.zig");

test "dbg statement of a method call on a rejected dispatch result lowers to LIR without compiler panic" {
    try harness.expectLowersToLirWithOptions(
        \\Q := { n : U64 }.{
        \\    grow : Q, U64 -> Q
        \\    grow = |Q.(q), k| Q.({ n: q.n + k })
        \\
        \\    size : Q -> U64
        \\    size = |Q.(q)| q.n
        \\}
        \\
        \\helper = |q| q.grow(1, 2)
        \\
        \\answer = {
        \\    dbg helper(Q.({ n: 0 })).size()
        \\    1
        \\}
        \\
        \\main! = |_args| {
        \\    dbg answer
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}
