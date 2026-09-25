# Row subsumption for a coerced FUNCTION used as a value (design.md "Row
# Subsumption", "Result-Row Widening Adapter"). Each `fwd*` returns a closed
# value it was handed, so its result row is coerced; every use below carries
# the function somewhere other than a direct call—a callable alias, a
# higher-order argument, a box, a record field—and some widen its row.
#
# Every widened row lists tags that sort AROUND the declared ones, so the
# declared and requested discriminants differ for every tag and a missing or
# misordered re-tag shows up as a wrong answer through `show*`.
RowSubsumptionCallableValue := {}

fwd : [B(Str), D] -> [B(Str), D]
fwd = |t| t

fwd_try : Try(Str, [NotFound]) -> Try(Str, [NotFound])
fwd_try = |t| t

fwd_generic : a, [B(Str), D] -> [B(Str), D]
fwd_generic = |_, t| t

show_wide : [A, B(Str), C, D] -> Str
show_wide = |v| match v { A => "A", B(s) => "B(${s})", C => "C", D => "D" }

show_narrow : [B(Str), D] -> Str
show_narrow = |v| match v { B(s) => "B(${s})", D => "D" }

show_try : Try(Str, [Gone, NotFound]) -> Str
show_try = |v| match v { Ok(s) => "Ok(${s})", Err(Gone) => "Gone", Err(NotFound) => "NotFound" }

# A top-level callable alias generalizes the re-opened row, so each of its
# uses picks its own width.
run_top = fwd
run_top_try = fwd_try
run_top_generic = fwd_generic

expect show_wide(run_top(B("x"))) == "B(x)"
expect show_narrow(run_top(D)) == "D"
expect show_try(run_top_try(Err(NotFound))) == "NotFound"
expect show_wide(run_top_generic(1, D)) == "D" and show_narrow(run_top_generic("s", B("g"))) == "B(g)"

# A local callable alias, used at two widths.
expect {
    run = fwd
    show_wide(run(B("l"))) == "B(l)" and show_narrow(run(D)) == "D"
}

# A higher-order argument at the wider and at the declared function type.
apply_wide : ([B(Str), D] -> [A, B(Str), C, D]), [B(Str), D] -> [A, B(Str), C, D]
apply_wide = |f, x| f(x)

apply_narrow : ([B(Str), D] -> [B(Str), D]), [B(Str), D] -> [B(Str), D]
apply_narrow = |f, x| f(x)

expect show_wide(apply_wide(fwd, D)) == "D"
expect show_narrow(apply_narrow(fwd, B("n"))) == "B(n)"

# Boxed at the wider type, boxed at the declared type, and boxed with the
# re-opened tail left for the use to decide.
expect {
    boxed : Box(([B(Str), D] -> [A, B(Str), C, D]))
    boxed = Box.box(fwd)
    show_wide(Box.unbox(boxed)(B("b"))) == "B(b)"
}

expect {
    boxed : Box(([B(Str), D] -> [B(Str), D]))
    boxed = Box.box(fwd)
    show_narrow(Box.unbox(boxed)(D)) == "D"
}

expect {
    boxed = Box.box(fwd)
    show_wide(Box.unbox(boxed)(D)) == "D"
}

# A record field at the wider type.
expect {
    rec = { f: fwd }
    show_wide((rec.f)(B("r"))) == "B(r)"
}
