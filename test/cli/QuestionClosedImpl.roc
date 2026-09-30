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

# `seal` forwards its closed input, which closes its output row, and so the
# row of every value built from it (design.md "Deferred: Row Subsumption").
# This depends on that known limitation (forwarding closes the row): once
# row subsumption lands, this fixture must close its impl row another way.
seal : Try(Str, [NotFound]) -> Try(Str, [NotFound])
seal = |v| v

closed_try : Try(Str, [NotFound])
closed_try = seal(Ok("hit"))

Src := [S].{
    fetch : Src -> Try(Str, [NotFound])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
