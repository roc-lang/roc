# Dispatch-cycle matrix (plan.md item 6/6a): a three-group dispatch chain
# whose back-edge closes only at the third group. `alpha` dispatches to
# Hop.once (group 2), whose body dispatches to Hop.twice (group 3), whose body
# calls back into the suspended `alpha` — Invariant D must keep the first two
# groups open (neither seals while its obligation is outstanding) so all three
# merge and generalize together.
Hop := [Mk(U64)].{
    once = |h, k| h.twice(k)

    twice = |h, k| match h {
        Hop.Mk(n) => if k == 0 { n } else { alpha(h, k - 1) }
    }
}

alpha = |h, k| h.once(k)

# NOTE: check-only for now — see DispatchCyclePhantomMerge.roc (pre-existing
# postcheck evidence bug on recursive dispatch, reproduces on main).
main! = |args| {
    result = alpha(Hop.Mk(11), List.len(args) + 4)
    if result != 11 {
        crash "three-group dispatch chain returned the wrong value"
    }
    Ok({})
}
