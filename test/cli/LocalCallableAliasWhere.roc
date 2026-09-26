## Generalized LOCAL callable aliases (`run = fwd`) of `where`-clause
## functions. Every use of an alias instantiates the alias's own scheme, which
## need not be its target's: an annotated alias can merge the target's
## variables or order its constraints differently. Boxy lowers each alias
## through one adapter generalized at the alias's scope.
LocalCallableAliasWhere := {}

Loc := [Loc].{
    get : Loc -> Str
    get = |_| "loc"
    name : Loc -> Str
    name = |_| "locname"
}

Other := [Other].{
    get : Other -> Str
    get = |_| "other"
    name : Other -> Str
    name = |_| "othername"
}

loc : Loc
loc = Loc

other : Other
other = Other

fwd : a, [B, C] -> [B, C] where [a.get : a -> Str]
fwd = |x, t| {
    _s = x.get()
    t
}

getter : a -> Str where [a.get : a -> Str]
getter = |x| x.get()

apply : (a -> Str), a -> Str
apply = |f, x| f(x)

pair2 : a, b -> Str where [a.get : a -> Str, b.get : b -> Str]
pair2 = |x, y| "${x.get()}+${y.get()}"

both : a, b -> Str where [b.get : b -> Str, a.name : a -> Str]
both = |x, y| "${x.name()}|${y.get()}"

expect {
    run = fwd
    run(loc, C) == C
}

expect {
    run = fwd
    again = run
    again(loc, B) == B and run(other, C) == C
}

expect {
    g = getter
    g(loc) == "loc" and g(other) == "other"
}

expect {
    g = getter
    apply(g, loc) == "loc" and apply(g, other) == "other"
}

expect {
    run = fwd
    wide : [A, B, C]
    wide = run(loc, C)
    match wide { A => Bool.False, B => Bool.False, C => Bool.True }
}

expect {
    run : x, x -> Str where [x.get : x -> Str]
    run = pair2
    run(loc, loc) == "loc+loc" and run(other, other) == "other+other"
}

expect {
    run : q, p -> Str where [p.get : p -> Str, q.name : q -> Str]
    run = both
    run(other, loc) == "othername|loc"
}

expect {
    run = both
    run(loc, other) == "locname|other"
}
