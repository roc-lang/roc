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

expect check_direct({})
expect check_try({})
expect check_rec({})
