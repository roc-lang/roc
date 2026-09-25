# Row subsumption for a forwarder whose signature has a `where` clause
# (design.md "Row Subsumption", "Result-Row Widening Adapter"). Each use below
# resolves the `where` evidence to a LOCAL procedure (`Loc.get`), so it is
# lowered as a caller-owned specialization, and its widened row is served by
# an adapter built in the caller's draft.
#
# Every widened row lists tags that sort AROUND the declared ones, so the
# declared and requested discriminants differ for every tag and a missing or
# misordered re-tag shows up as a wrong answer through `show*`.
RowSubsumptionWhereForwarder := {}

# The direct result row: declared `[B(Str), D]` numbers `B` 0 and `D` 1, the
# widened `[A, B(Str), C, D]` numbers them 1 and 3.
fwd_direct : a, [B(Str), D] -> [B(Str), D] where [a.get : a -> Str]
fwd_direct = |x, t| {
    _s = x.get()
    t
}

show_direct : [A, B(Str), C, D] -> Str
show_direct = |v| match v { A => "A", B(s) => "B(${s})", C => "C", D => "D" }

# The error row of a `Try` standing as the result: `NotFound` is 0 in the
# declared row and 1 in the widened `[Gone, NotFound]`.
fwd_try : a, Try(Str, [NotFound]) -> Try(Str, [NotFound]) where [a.get : a -> Str]
fwd_try = |x, t| {
    _s = x.get()
    t
}

show_try : Try(Str, [Gone, NotFound]) -> Str
show_try = |v| match v { Ok(s) => "Ok(${s})", Err(Gone) => "Gone", Err(NotFound) => "NotFound" }

# A recursive forwarder: its recursive reference is at the declared row.
fwd_rec : a, [B(Str), D], U64 -> [B(Str), D] where [a.get : a -> Str]
fwd_rec = |x, t, n| {
    _s = x.get()
    if n == 0 t else fwd_rec(x, t, n - 1)
}

check_direct : {} -> Bool
check_direct = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    wide : Loc, [B(Str), D] -> [A, B(Str), C, D]
    wide = |l, t| fwd_direct(l, t)

    show_direct(wide(Loc.L, B("x"))) == "B(x)" and show_direct(wide(Loc.L, D)) == "D"
}

check_try : {} -> Bool
check_try = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    wide : Loc, Try(Str, [NotFound]) -> Try(Str, [Gone, NotFound])
    wide = |l, t| fwd_try(l, t)

    show_try(wide(Loc.L, Err(NotFound))) == "NotFound" and show_try(wide(Loc.L, Ok("y"))) == "Ok(y)"
}

check_rec : {} -> Bool
check_rec = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    wide : Loc, [B(Str), D] -> [A, B(Str), C, D]
    wide = |l, t| fwd_rec(l, t, 3)

    show_direct(wide(Loc.L, B("r"))) == "B(r)" and show_direct(wide(Loc.L, D)) == "D"
}

# Two widened uses in one body, then a let-bound use followed by another: a
# where-clause forwarder is a partial scheme whose uses share its ground row,
# and a literal argument at one use restructures that row for the next.
check_twice : {} -> Bool
check_twice = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    show_direct(fwd_direct(Loc.L, B("p"))) == "B(p)" and show_direct(fwd_direct(Loc.L, D)) == "D"
}

check_let : {} -> Bool
check_let = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    first = fwd_direct(Loc.L, D)
    show_direct(first) == "D" and show_direct(fwd_direct(Loc.L, B("q"))) == "B(q)"
}

# Two uses in one body of a forwarder's `Try` error row. `Gone` sorts first,
# so every declared error tag moves in the requested row.
fwd_try2 : a, Try(Str, [Missing, NotFound]) -> Try(Str, [Missing, NotFound]) where [a.get : a -> Str]
fwd_try2 = |x, t| {
    _s = x.get()
    t
}

show_try2 : Try(Str, [Gone, Missing, NotFound]) -> Str
show_try2 = |v| match v { Ok(s) => "Ok(${s})", Err(Gone) => "Gone", Err(Missing) => "Missing", Err(NotFound) => "NotFound" }

check_try_twice : {} -> Bool
check_try_twice = |_| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    show_try2(fwd_try2(Loc.L, Err(NotFound))) == "NotFound" and show_try2(fwd_try2(Loc.L, Err(Missing))) == "Missing"
}

# A widened row carrying an iterator produced in the caller.
fwd_iter : a, [Some(Iter(U64)), None] -> [Some(Iter(U64)), None] where [a.get : a -> Str]
fwd_iter = |x, t| {
    _s = x.get()
    t
}

sum_iter : [Some(Iter(U64)), None, Extra] -> U64
sum_iter = |v| match v {
    Some(it) => List.from_iter(it).sum()
    None => 0
    Extra => 99
}

check_iter : List(U64) -> Bool
check_iter = |xs| {
    Loc := [L].{
        get : Loc -> Str
        get = |_| "loc"
    }

    sum_iter(fwd_iter(Loc.L, Some(xs.iter().map(|v| v + 1)))) == 9 and sum_iter(fwd_iter(Loc.L, None)) == 0
}

expect check_direct({})
expect check_iter([1, 2, 3])
expect check_try({})
expect check_rec({})
expect check_twice({})
expect check_let({})
expect check_try_twice({})

# A top-level callable alias of a where-clause forwarder, used with a
# top-level evidence type at the wider row.
TopLoc := [T].{
    get : TopLoc -> Str
    get = |_| "top"
}

run_where = fwd_direct

expect show_direct(run_where(TopLoc.T, B("w"))) == "B(w)" and show_direct(run_where(TopLoc.T, D)) == "D"
