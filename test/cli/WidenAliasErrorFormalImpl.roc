# polarity_phase_two.md W6b: the adapter-reachable row written as an alias's
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

closed_try : Res([IoErr])
closed_try = Ok("hit")

Src := [S].{
    fetch : Src -> Res([IoErr])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
