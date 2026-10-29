//! Regression tests for https://github.com/roc-lang/roc/issues/12617.
//!
//! A call whose result flows into a row wider than the one the checker
//! recorded for it, because a `?` composed more tags into that row, must
//! check and lower cleanly through post-check monotype when the callee's
//! error type is a bare, rigid or `is_eq`-constrained variable.
const harness = @import("lower_to_lir_harness.zig");

const record_hook_branch_app =
    \\hooks = { run: |x| x }
    \\
    \\main! = |_args| {
    \\    _ = handle(1)?
    \\    Ok({})
    \\}
    \\
    \\handle = |n|
    \\    if (hooks.run)(n) == 1 {
    \\        Err(Failed)?
    \\        Ok("a")
    \\    } else {
    \\        ok("b")
    \\    }
    \\
    \\ok : Str -> Try(Str, _)
    \\ok = |s| Ok(s)
;

const lambda_arg_app =
    \\parse : Str -> Try(Str, [Empty])
    \\parse = |s| if s == "" Err(Empty) else Ok(s)
    \\
    \\wrap : (() -> Try(Str, [Missing, ..others])) -> Try(Str, [Missing, ..others])
    \\wrap = |body| body()
    \\
    \\ok : Str -> Try(Str, err)
    \\ok = |s| Ok(s)
    \\
    \\run = |s| wrap(|| ok(parse(s)?))
    \\
    \\main! = |_args| {
    \\    _ = run("a")
    \\    Ok({})
    \\}
;

const constrained_rigid_app =
    \\main! = |_args| {
    \\    _ = handle(1)?
    \\    Ok({})
    \\}
    \\
    \\handle = |n|
    \\    if n == 1 {
    \\        Err(Failed)?
    \\        Ok("a")
    \\    } else {
    \\        ok("b")
    \\    }
    \\
    \\ok : Str -> Try(Str, e) where [e.is_eq : e, e -> Bool]
    \\ok = |s| Ok(s)
;

test "issue 12617: branch on a top-level record function mixing `?` with an open-row helper lowers" {
    try harness.expectLowersToLir(record_hook_branch_app);
}

test "issue 12617: `?` in the argument of an open-row helper inside a lambda lowers" {
    try harness.expectLowersToLir(lambda_arg_app);
}

test "issue 12617: a rigid error variable whose constraints a closed row discharges lowers" {
    try harness.expectLowersToLir(constrained_rigid_app);
}
