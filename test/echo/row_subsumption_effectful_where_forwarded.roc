# A where-clause requirement forwarded through two effectful functions
# (`fwd2!` to `fwd!`), discharged by a method of a type declared inside a
# function (`run!`) that another function drafts. The inner call's evidence
# is the enclosing chain's entry, whose target is consumed as recorded: the
# local type's method is visible only from its declaring function's scope.
fwd! : a, [B, C] => [B, C] where [a.get : a -> Str]
fwd! = |x, t| {
    _s = x.get()
    t
}

fwd2! : a, [B, C] => [B, C] where [a.get : a -> Str]
fwd2! = |x, t| fwd!(x, t)

show : [A, B, C] -> Str
show = |v| match v { A => "A", B => "B", C => "C" }

run! : {} => Bool
run! = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "run"
    }

    show(fwd2!(Loc.L, B)) == "B" and fwd2!(Loc.L, C) == C
}

main! = |_args| {
    if run!({}) {
        echo!("ok\n")
    } else {
        crash "incorrect forwarded local evidence"
    }
    Ok({})
}
