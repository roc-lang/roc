# polarity_phase_two.md W3: the same shape as OpenMethodWidenedCaller with the
# callee used only at its own row. The dispatch in `wrap` stays
# `direct_parametric`: the rule is about which scheme quantifies the row tail,
# not about whether any caller happens to widen it.
OpenMethodOwnRowCaller := {}

Rows := {}.{
    wrapped : Rows -> Try(Str, [Unavailable])
    wrapped = |_| Ok("x")
}

wrap : Rows -> Try(Str, [Unavailable])
wrap = |rows| rows.wrapped()

use : Rows -> Try(Str, [Unavailable])
use = |rows| {
    s = wrap(rows)?
    Ok(s)
}

expect {
    use(Rows.{}) == Ok("x")
}
