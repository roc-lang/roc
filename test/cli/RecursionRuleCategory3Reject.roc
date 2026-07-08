# Category-3 case (plan.md item 4/8): the binding-group recursion rule infers
# an unannotated recursive group monomorphically, so a member used at two
# incompatible types WITHIN its own group is a type error. This is the correct
# behavior — the old deferred validation could let the unsound scheme through.
# `roc check` must report a type mismatch (and must not panic).
poly_a = |x, n| if n == 0 { x } else { poly_b(x, n - 1) }

poly_b = |x, n| {
    _ = poly_a("pinned to Str in-group", n)
    poly_a(x, n)
}

main! = |_| {
    _ = poly_a(42, 1)
    Ok({})
}
