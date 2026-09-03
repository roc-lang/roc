# polarity_phase_two.md W3: the dispatch sits inside a generalized local
# function whose receiver is concrete (`rows2 : Rows`), so the plan is direct
# and its callable's row tail is quantified by the local's scheme (the
# scope-chain arm of the classification): `direct_parametric`, and the outer
# `?` widens the local's instantiation. (A dispatch on the local's own
# unconstrained parameter would be evidence-dependent instead.)
OpenMethodNestedLocalCaller := {}

Rows := {}.{
    wrapped : Rows -> Try(Str, [Unavailable])
    wrapped = |_| Ok("x")
}

use : Rows -> Try(Str, [Unavailable, Other])
use = |rows| {
    helper = |r| {
        rows2 : Rows
        rows2 = r
        rows2.wrapped()
    }
    s = helper(rows)?
    Ok(s)
}

expect {
    use(Rows.{}) == Ok("x")
}
