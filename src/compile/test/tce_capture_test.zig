//! Regression tests for tail-call elimination of a recursive closure that
//! captures a refcounted value.
//!
//! Plain TCE turns the proc's own argument locals into the loop join's
//! parameters and enters the loop with a bare `jump` (no `initialize_join_param`
//! writes — the argument values are already in place). When join-parameter
//! scalarization later splits a captured struct parameter into per-field
//! parameters, the field parameters must still be seeded on that entry path by
//! reading the argument struct's fields; otherwise the ARC borrow certifier
//! reports a "release of unbound local" for the never-initialized field on the
//! first loop iteration.

const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLir = harness.expectLowersToLir;
const expectDeterministicLir = harness.expectDeterministicLir;

test "tce capture: tail-recursive closure capturing a Dict lowers to LIR" {
    try expectLowersToLir(
        \\T :: {}.{
        \\    run : List(U8) -> I64
        \\    run = |vars| {
        \\        gains : Dict(U8, I64)
        \\        gains = Dict.from_list([(1, 3), (2, 4)])
        \\        recur : List(U8), I64 -> I64
        \\        recur = |xs, acc| {
        \\            match xs {
        \\                [] => acc
        \\                [x, .. as rest] => recur(rest, acc + (gains.get(x) ?? 0))
        \\            }
        \\        }
        \\        recur(vars, 0)
        \\    }
        \\}
        \\
        \\main! = |_args| {
        \\    _ = T.run([1, 2, 1])
        \\    Ok({})
        \\}
    );
}

test "tce capture: tail-recursive closure capturing two Dicts lowers to LIR" {
    try expectLowersToLir(
        \\TT :: {}.{
        \\    run : List(U8) -> I64
        \\    run = |vars| {
        \\        a : Dict(U8, I64)
        \\        a = Dict.from_list([(1, 100), (2, 200)])
        \\        b : Dict(U8, I64)
        \\        b = Dict.from_list([(1, 1), (2, 2)])
        \\        recur : List(U8), I64 -> I64
        \\        recur = |xs, acc| {
        \\            match xs {
        \\                [] => acc
        \\                [x, .. as rest] => recur(rest, acc + (a.get(x) ?? 0) + (b.get(x) ?? 0))
        \\            }
        \\        }
        \\        recur(vars, 0)
        \\    }
        \\}
        \\
        \\main! = |_args| {
        \\    _ = TT.run([1, 2])
        \\    Ok({})
        \\}
    );
}

test "tce capture: tail-recursive closure capturing a List lowers to LIR" {
    try expectLowersToLir(
        \\TL :: {}.{
        \\    run : List(U8) -> I64
        \\    run = |vars| {
        \\        weights : List(I64)
        \\        weights = [10, 20, 30]
        \\        recur : List(U8), I64 -> I64
        \\        recur = |xs, acc| {
        \\            match xs {
        \\                [] => acc
        \\                [_, .. as rest] => recur(rest, acc + (weights.first() ?? 0))
        \\            }
        \\        }
        \\        recur(vars, 0)
        \\    }
        \\}
        \\
        \\main! = |_args| {
        \\    _ = TL.run([1, 2, 3, 4])
        \\    Ok({})
        \\}
    );
}

test "tce capture: nested lambda inside a tail-recursive capturing closure lowers to LIR" {
    try expectLowersToLir(
        \\N2 :: {}.{
        \\    run : List(U8) -> I64
        \\    run = |vars| {
        \\        gains : Dict(U8, I64)
        \\        gains = Dict.from_list([(1, 3), (2, 4)])
        \\        recur : List(U8), I64 -> I64
        \\        recur = |xs, acc| {
        \\            match xs {
        \\                [] => acc
        \\                [x, .. as rest] => {
        \\                    bonus = [10, 20].map(|m| m + (gains.get(x) ?? 0))
        \\                    recur(rest, acc + (bonus.first() ?? 0))
        \\                }
        \\            }
        \\        }
        \\        recur(vars, 0)
        \\    }
        \\}
        \\
        \\main! = |_args| {
        \\    _ = N2.run([1, 2])
        \\    Ok({})
        \\}
    );
}

test "tce capture: capture-fixpoint lowering is deterministic" {
    // Lambda lifting solves capture sets with an edge-driven worklist and no
    // separate normalizing pass; this guards that the resulting LIR (capture
    // order included) does not depend on scheduling or iteration order.
    try expectDeterministicLir(
        \\Solver :: {}.{
        \\    solve : List(U8) -> Try(List((U8, U8)), [Bad])
        \\    solve = |vars| {
        \\        equation : Dict(U8, I64)
        \\        equation = Dict.from_list([(1, 1), (2, -1)])
        \\        find_match : List((U8, U8)), List(U8), Set(U8) -> Try(List((U8, U8)), [Bad])
        \\        find_match = |assignments, remaining_vars, remaining_digits| {
        \\            match remaining_vars {
        \\                [] => {
        \\                    total : I64
        \\                    total = assignments.fold(0, |acc, (letter, value)| (equation.get(letter) ?? 0) * value.to_i64() + acc)
        \\                    if total != 0 { Err(Bad) } else { Ok(assignments) }
        \\                }
        \\                [letter, .. as rest] => {
        \\                    first_ok(remaining_digits, |digit| {
        \\                        find_match(assignments.append((letter, digit)), rest, remaining_digits)
        \\                    })
        \\                }
        \\            }
        \\        }
        \\        find_match([], vars, Set.from_list([0, 1, 2, 3]))
        \\    }
        \\}
        \\
        \\first_ok : Set(a), (a -> Try(b, err)) -> Try(b, [Bad])
        \\first_ok = |set, func| {
        \\    set.to_list().fold_until(Err(Bad), |state, elem| {
        \\        match func(elem) {
        \\            Err(_) => Continue(state)
        \\            Ok(val) => Break(Ok(val))
        \\        }
        \\    })
        \\}
        \\
        \\main! = |_args| {
        \\    _ = Solver.solve([1, 2])
        \\    Ok({})
        \\}
    );
}
