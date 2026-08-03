# Dispatch-cycle matrix (plan.md item 6/6a): the premature-generalization
# trap. An earlier group (`outer`) dispatches into a later group (Wrap.enter),
# which dispatches back into `outer` at a DIFFERENT call shape. If `outer`
# could seal while its obligation on Wrap.enter was outstanding, the back-edge
# could no longer absorb the merged facts; Invariant D forbids sealing until
# the obligation resolves, so this must simply type-check and run.
Wrap := [Mk(U64)].{
    enter = |w, depth| match w {
        Wrap.Mk(n) => if depth == 0 { n } else { outer(w, depth - 1) }
    }
}

outer = |w, depth| {
    stepped = w.enter(depth)
    stepped + 0
}

main! = |args| {
    result = outer(Wrap.Mk(5), List.len(args) + 6)
    if result != 5 {
        crash "premature-generalization trap returned the wrong value"
    }
    Ok({})
}
