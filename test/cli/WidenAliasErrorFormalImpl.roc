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

# `closed_value` is deliberately UNANNOTATED. An annotated value's implicitly
# opened row is quantified now (design.md "Polarity"), so an annotation can no
# longer produce a closed row at all. `closed` takes the row in an INPUT
# position, where it is generated as written, and returns it, so its result row
# is bound to `[]` by its own body: an input-position parameter is one of the
# closed sources design.md names. `closed_value` is therefore still a top-level
# constant whose row is closed, which is what this fixture needs.
closed : Res([IoErr]) -> Res([IoErr])
closed = |v| v

closed_try = closed(Ok("hit"))

Src := [S].{
    fetch : Src -> Res([IoErr])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
