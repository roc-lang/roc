# The `Try` instance of the result-row widening adapter. `fetch` publishes
# the closed error row `[NotFound]`; `?` inside `load` requests
# `[NotFound, Other]`. The adapter unwraps the declared-row `Try` and re-wraps
# its error into the wider row. A `Try`'s ERROR row is the only adaptable
# nested position—its OK row is not (see the checker rejection in
# `type_checking_integration.zig`).
QuestionClosedImpl := {}

load : a -> Try(Str, [NotFound, Other]) where [a.fetch : a -> Try(Str, [NotFound])]
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
Closed := { v : Try(Str, [NotFound]) }

closed_try = Closed.{ v: Ok("hit") }.v

Src := [S].{
    fetch : Src -> Try(Str, [NotFound])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
