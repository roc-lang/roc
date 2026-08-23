//! Regression test for issue #10710.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 10710: compile-time constant list of functions built by List.map" {
    try expectLowersToLir(
        \\rules : List(() -> U8)
        \\rules = [|| 5].map(|rule| rule)
        \\
        \\main! = |_args| {
        \\    first = match rules {
        \\        [rule, ..] => rule()
        \\        [] => 0
        \\    }
        \\    echo!(first.to_str())
        \\    Ok({})
        \\}
    );
}
