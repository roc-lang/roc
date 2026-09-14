# polarity_phase_two.md W6b: the `Try` instance of the same mechanism. `fetch`
# publishes the closed error row `[NotFound]`; `?` inside `load` requests
# `[NotFound, Other]`. The adapter unwraps the declared-row `Try` and re-wraps
# its error into the wider row. A `Try`'s ERROR row is the only adaptable
# nested position — its OK row is not (see the checker rejection in
# `type_checking_integration.zig`).
QuestionClosedImpl := {}

load : a -> Try(Str, [NotFound, Other]) where [a.fetch : a -> Try(Str, [NotFound])]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

closed_try : Try(Str, [NotFound])
closed_try = Ok("hit")

Src := [S].{
    fetch : Src -> Try(Str, [NotFound])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
