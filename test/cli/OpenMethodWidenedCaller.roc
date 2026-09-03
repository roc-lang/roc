# polarity_phase_two.md W3: a method whose implicitly open error row the
# enclosing function's own return row shares is a `direct_parametric` plan.
# `use` widens `wrap`'s row through `?`, so classifying the dispatch in `wrap`
# closed would seal the tail to `[]` and fail Monotype's exact unification
# with the widened request (`instantiation widened a closed tag union`).
OpenMethodWidenedCaller := {}

Rows := {}.{
    wrapped : Rows -> Try(Str, [Unavailable])
    wrapped = |_| Ok("x")
}

wrap : Rows -> Try(Str, [Unavailable])
wrap = |rows| rows.wrapped()

use : Rows -> Try(Str, [Unavailable, Other])
use = |rows| {
    s = wrap(rows)?
    Ok(s)
}

expect {
    use(Rows.{}) == Ok("x")
}
