# Dispatch-cycle matrix (plan.md item 6/6a): the same phantom-cycle shape as
# DispatchCyclePhantomMerge, but with the method annotated. The annotated
# member's scheme is pre-declared, so ping's dispatch obligation resolves
# against the scheme without checking the body — the cycle is cut, no merge.
Counter := [Mk(U64)].{
    step : Counter, U64 -> U64
    step = |c, k| match c {
        Counter.Mk(n) => if k == 0 { n } else { ping(c, k - 1) }
    }
}

ping = |c, k| c.step(k)

main! = |args| {
    result = ping(Counter.Mk(9), List.len(args) + 2)
    if result != 9 {
        crash "annotated dispatch cycle returned the wrong value"
    }
    Ok({})
}
