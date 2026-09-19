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

# `closed_value` is deliberately UNANNOTATED. An annotated value's implicitly
# opened row is quantified now (design.md "Polarity"), so an annotation can no
# longer produce a closed row at all. `closed` takes the row in an INPUT
# position, where it is generated as written, and returns it, so its result row
# is bound to `[]` by its own body: an input-position parameter is one of the
# closed sources design.md names. `closed_value` is therefore still a top-level
# constant whose row is closed, which is what this fixture needs.
closed : Try(Str, [NotFound]) -> Try(Str, [NotFound])
closed = |v| v

closed_try = closed(Ok("hit"))

Src := [S].{
    fetch : Src -> Try(Str, [NotFound])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
