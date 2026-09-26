# The adapter-reachable row written as an alias's
# type ARGUMENT. `Res(e) : Try(Str, e)` puts `e` in the `Try` error position,
# which the result-row widening adapter re-tags, so the row written at the
# reference (`Res([IoErr])`) has to be opened per use exactly as a directly
# written `Try(Str, [IoErr])` is.
#
# Before `applyTryErrorArgIndex` crossed the alias, `annoApplyIsBuiltinTry`
# matched only a DIRECT builtin `Try`, so every argument of `Res([IoErr])` was
# generated out of reach and closed. Lowering crosses the same alias and would
# have adapted the row, so the opened set was strictly smaller than the
# adaptable set and this program failed to check.
Res(e) : Try(Str, e)

load : a -> Res([IoErr, Other]) where [a.fetch : a -> Res([IoErr])]
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
Closed := { v : Res([IoErr]) }

closed_try = Closed.{ v: Ok("hit") }.v

Src := [S].{
    fetch : Src -> Res([IoErr])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
