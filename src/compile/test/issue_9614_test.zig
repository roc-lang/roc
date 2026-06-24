//! Regression test for issue #9614: a fold_until accumulator whose open tag
//! union is widened across dispatch-plan positions must lower to LIR.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 9614: default app list fold_until result tag narrowing lowers to LIR" {
    try expectLowersToLir(
        \\solve = |problem| {
        \\    find_match = |chars| {
        \\        match chars {
        \\            [] => Err(InvalidAssignment)
        \\            [_, ..] => {
        \\                find_first_ok(chars, |_| Err(InvalidAssignment))
        \\            }
        \\        }
        \\    }
        \\    find_match(problem.to_utf8())
        \\}
        \\
        \\find_first_ok = |list, func| {
        \\    list.fold_until(
        \\        Err(InvalidAssignment),
        \\        |state, elem| {
        \\            match func(elem) {
        \\                Err(_) => Continue(state)
        \\                Ok(val) => Break(Ok(val))
        \\            }
        \\        },
        \\    )
        \\}
        \\
        \\main! = |_args| {
        \\    _ = solve("abc")
        \\    Ok({})
        \\}
    );
}
