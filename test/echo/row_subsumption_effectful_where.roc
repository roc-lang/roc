# Row subsumption for an EFFECTFUL forwarder whose signature has a `where`
# clause (design.md "Row Subsumption", "Result-Row Widening Adapter"). Each
# widened use is served by a re-tagging adapter; `A` sorts before the declared
# tags, so a missing or misordered re-tag reports the wrong tag.
fwd! : a, [B, C] => [B, C] where [a.get : a -> Str]
fwd! = |x, t| {
    _s = x.get()
    t
}

Top := [T].{
    get : Top -> Str
    get = |_| "top"
}

show : [A, B, C] -> Str
show = |v| match v { A => "A", B => "B", C => "C" }

main! = |_args| {
    first = show(fwd!(Top.T, B))
    second = show(fwd!(Top.T, C))
    if first == "B" and second == "C" {
        echo!("ok\n")
    } else {
        crash "incorrect effectful row subsumption re-tag"
    }
    Ok({})
}
