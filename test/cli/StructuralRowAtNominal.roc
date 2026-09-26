## Structural tag rows and records returned by unannotated generalized functions, used
## where the checker unified them with a nominal type's backing (`Try`, and a
## user nominal with a type argument). Boxy aligns the structural worker row
## with the call nominal's backing, whose declaration formals it binds to the
## call's actual arguments.
StructuralRowAtNominal := {}

Res(a) := [Good(a), Bad(Str), Gone]

nf = |_| Err(NotFound)
ok_s = |s| Ok(s)
nested = |_| Ok(Err(Inner))
bad = |s| Bad(s)
good = |x| Good(x)
mk = |x| if x == x Ok(x) else Err(NotFound)
mk_err = |x| if x == x Err(NotFound) else Ok(x)

shown : Try(Str, [Missing, NotFound]) -> Str
shown = |v| match v { Ok(s) => s, Err(Missing) => "Missing", Err(NotFound) => "NotFound" }

shown_nested : Try(Try(U64, [Inner, Outer]), [Top]) -> Str
shown_nested = |v| match v { Ok(Ok(n)) => n.to_str(), Ok(Err(Inner)) => "Inner", Ok(Err(Outer)) => "Outer", Err(Top) => "Top" }

shown_res : Res(U64) -> Str
shown_res = |v| match v { Good(n) => n.to_str(), Bad(s) => "Bad(${s})", Gone => "Gone" }

expect shown(nf({})) == "NotFound"
expect shown(ok_s("hi")) == "hi"
expect shown_nested(nested({})) == "Inner"
expect shown_res(bad("x")) == "Bad(x)"
expect shown_res(good(7)) == "7"
expect shown(mk("a")) == "a"
expect shown(mk_err("a")) == "NotFound"

expect {
    t : Try(Str, [Missing, NotFound])
    t = nf({})
    Str.inspect(t) == "Err(NotFound)"
}

expect {
    t : Try(Str, [Missing, NotFound])
    t = nf({})
    t == Err(NotFound)
}

P(a) := { x : a, s : Str }

Nested(a) := [Good(Try(a, Str)), Gone]

mk_p = |v| { x: v, s: "t" }
mk_p_const = |_| { x: "c", s: "t" }
nested_good = |v| Good(Ok(v))
nested_bad = |_| Good(Err("bad"))

shown_p : P(Str) -> Str
shown_p = |p| Str.concat(p.x, p.s)

shown_nested_res : Nested(U64) -> Str
shown_nested_res = |v| match v { Good(Ok(n)) => n.to_str(), Good(Err(e)) => e, Gone => "Gone" }

expect shown_p(mk_p("a")) == "at"
expect shown_p(mk_p_const({})) == "ct"
expect shown_nested_res(nested_good(3)) == "3"
expect shown_nested_res(nested_bad({})) == "bad"
