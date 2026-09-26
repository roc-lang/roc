# Row subsumption for an EFFECTFUL forwarder whose `where` clause is satisfied
# by a method of a type declared inside the calling functions (`main!` and
# `run!`). Relating each caller as another function's callee consumes the
# checked target of that evidence; the local type's method is visible only
# from its declaring function's scope. `A` sorts before the declared tags, so
# a missing or misordered re-tag reports the wrong tag.
fwd! : a, [B, C] => [B, C] where [a.get : a -> Str]
fwd! = |x, t| {
    _s = x.get()
    t
}

show : [A, B, C] -> Str
show = |v| match v { A => "A", B => "B", C => "C" }

run! : {} => Bool
run! = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "run"
    }

    narrow = fwd!(Loc.L, B)
    show(fwd!(Loc.L, B)) == "B" and show(fwd!(Loc.L, C)) == "C" and narrow == B
}

main! = |_args| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "main"
    }

    first = show(fwd!(Loc.L, B))
    unwidened = fwd!(Loc.L, C)
    if first == "B" and unwidened == C and run!({}) {
        echo!("ok\n")
    } else {
        crash "incorrect effectful row subsumption with local evidence"
    }
    Ok({})
}
