//! Regression tests for issue #9634: a nested closure that transitively needs
//! an outer capture — because it references a sibling/recursive closure that
//! captures that value — must have the capture threaded into its own capture
//! set. Lambda lifting now solves capture sets as a least fixed point over the
//! function-reference graph (independent of lifting order and rewrite-collapsed
//! nodes), so these programs lower to LIR without the ARC borrow certifier
//! reporting a "use of unbound refcounted local".

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 9634: captured Dict in recursive fold_until closure lowers to LIR" {
    try expectLowersToLir(
        \\Alphametics :: {}.{
        \\    solve : List(U8) -> Try(List((U8, U8)), [InvalidAssignment])
        \\    solve = |vars| {
        \\        equation : Dict(U8, I64)
        \\        equation = Dict.from_list([(1, 1), (2, -1)])
        \\
        \\        find_match : List((U8, U8)), List(U8), Set(U8) -> Try(List((U8, U8)), [InvalidAssignment])
        \\        find_match = |assignments, remaining_vars, remaining_digits| {
        \\            match remaining_vars {
        \\                [] => {
        \\                    total_val : I64
        \\                    total_val =
        \\                        assignments.fold(
        \\                            0,
        \\                            |total, (letter, value)| {
        \\                                (equation.get(letter) ?? 0) * value.to_i64() + total
        \\                            },
        \\                        )
        \\
        \\                    if total_val != 0 { Err(InvalidAssignment) } else { Ok(assignments) }
        \\                }
        \\                [letter, .. as rest] => {
        \\                    find_first_ok(
        \\                        remaining_digits,
        \\                        |digit| {
        \\                            find_match(assignments.append((letter, digit)), rest, remaining_digits)
        \\                        }
        \\                    )
        \\                }
        \\            }
        \\        }
        \\
        \\        digits = Set.from_list([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
        \\        find_match([], vars, digits)
        \\    }
        \\}
        \\
        \\find_first_ok : Set(a), (a -> Try(b, err)) -> Try(b, [InvalidAssignment])
        \\find_first_ok = |set, func| {
        \\    set.to_list().fold_until(
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
        \\    _ = Alphametics.solve([1, 2])
        \\    Ok({})
        \\}
    );
}

test "issue 9634: recursive closure with two refcounted captures lowers to LIR" {
    // `walk` captures two outer Dicts and recurses through `choose`'s lambda,
    // so the lambda must transitively capture both `weights` and `offsets`.
    try expectLowersToLir(
        \\Two :: {}.{
        \\    run : List(U8) -> Try(List(U8), [Bad])
        \\    run = |vars| {
        \\        weights : Dict(U8, I64)
        \\        weights = Dict.from_list([(1, 5), (2, 7)])
        \\        offsets : Dict(U8, I64)
        \\        offsets = Dict.from_list([(1, 1), (2, 2)])
        \\
        \\        walk : List(U8), List(U8) -> Try(List(U8), [Bad])
        \\        walk = |acc, remaining| {
        \\            match remaining {
        \\                [] => Ok(acc)
        \\                [v, .. as rest] => {
        \\                    choose(
        \\                        [0, 1, 2],
        \\                        |d| {
        \\                            score = (weights.get(v) ?? 0) * d.to_i64() + (offsets.get(v) ?? 0)
        \\                            if score > 100 {
        \\                                Err(Bad)
        \\                            } else {
        \\                                walk(acc.append(v), rest)
        \\                            }
        \\                        },
        \\                    )
        \\                }
        \\            }
        \\        }
        \\        walk([], vars)
        \\    }
        \\}
        \\
        \\choose : List(U8), (U8 -> Try(a, [Bad])) -> Try(a, [Bad])
        \\choose = |options, f| {
        \\    options.fold_until(
        \\        Err(Bad),
        \\        |state, opt| {
        \\            match f(opt) {
        \\                Err(_) => Continue(state)
        \\                Ok(v) => Break(Ok(v))
        \\            }
        \\        },
        \\    )
        \\}
        \\
        \\main! = |_args| {
        \\    _ = Two.run([1, 2])
        \\    Ok({})
        \\}
    );
}

test "issue 9634: multi-hop transitive capture through sibling closures lowers to LIR" {
    // The `|_|` lambda calls recursive `walk`, which calls sibling `score`,
    // which captures `base`. `base` must propagate two hops up the
    // function-reference graph into the innermost lambda's capture set.
    try expectLowersToLir(
        \\Hop :: {}.{
        \\    run : List(U8) -> Try(I64, [Bad])
        \\    run = |vars| {
        \\        base : Dict(U8, I64)
        \\        base = Dict.from_list([(1, 2), (2, 3)])
        \\
        \\        score : U8 -> I64
        \\        score = |v| (base.get(v) ?? 0) * 10
        \\
        \\        walk : List(U8), I64 -> Try(I64, [Bad])
        \\        walk = |xs, acc| {
        \\            match xs {
        \\                [] => Ok(acc)
        \\                [v, .. as rest] => {
        \\                    pick(
        \\                        [0, 1],
        \\                        |_| {
        \\                            next = acc + score(v)
        \\                            if next > 1000 {
        \\                                Err(Bad)
        \\                            } else {
        \\                                walk(rest, next)
        \\                            }
        \\                        },
        \\                    )
        \\                }
        \\            }
        \\        }
        \\        walk(vars, 0)
        \\    }
        \\}
        \\
        \\pick : List(U8), (U8 -> Try(a, [Bad])) -> Try(a, [Bad])
        \\pick = |opts, f| {
        \\    opts.fold_until(Err(Bad), |st, o| {
        \\        match f(o) {
        \\            Err(_) => Continue(st)
        \\            Ok(v) => Break(Ok(v))
        \\        }
        \\    })
        \\}
        \\
        \\main! = |_args| {
        \\    _ = Hop.run([1, 2])
        \\    Ok({})
        \\}
    );
}

test "issue 9634: captured Dict used in string interpolation inside a recursive closure lowers to LIR" {
    // String interpolation desugars to an inline lambda (the one IR shape that
    // stays an inline `.lambda` rather than a nested def). Exercising it inside
    // a recursive capturing closure covers lambda lifting's inline-descent path
    // for captures, which is separate from the def/nested-def fixpoint.
    try expectLowersToLir(
        \\IC :: {}.{
        \\    run : List(U8) -> Str
        \\    run = |vars| {
        \\        labels : Dict(U8, Str)
        \\        labels = Dict.from_list([(1, "one"), (2, "two")])
        \\        recur : List(U8), Str -> Str
        \\        recur = |xs, acc| {
        \\            match xs {
        \\                [] => acc
        \\                [x, .. as rest] => {
        \\                    name = labels.get(x) ?? "?"
        \\                    recur(rest, "${acc}${name},")
        \\                }
        \\            }
        \\        }
        \\        recur(vars, "")
        \\    }
        \\}
        \\
        \\main! = |_args| {
        \\    _ = IC.run([1, 2, 1])
        \\    Ok({})
        \\}
    );
}
