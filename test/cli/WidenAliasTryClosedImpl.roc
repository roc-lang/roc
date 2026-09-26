IoResult(a) : Try(a, [IoErr])

load : a -> Try(Str, [IoErr, Other]) where [a.fetch : a -> IoResult(Str)]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

# `closed_try` is deliberately UNANNOTATED, and its row is read out of a nominal
# field. Neither an annotation nor a forwarding function can produce a closed
# row any more: an annotated value's implicitly opened row is quantified
# (design.md "Polarity"), and a top-level function that FORWARDS a closed value
# has its result row coerced open again at every use (design.md "Row
# Subsumption"). A nominal declaration's body closes its rows as written, so a
# field of `Closed` is a closed source that no coercion reopens, and `closed_try`
# is a top-level constant whose row is closed, which is what this fixture needs.
Closed := { v : IoResult(Str) }

closed_try = Closed.{ v: Ok("hit") }.v

Src := [S].{
    fetch : Src -> IoResult(Str)
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
