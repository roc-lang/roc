# polarity_phase_two.md W3: an explicit extra tag with a NAMED extension. A
# named extension is a rigid, so the dispatch in `wrap` is `direct_parametric`
# by the rigid arm of the classification, independent of row defaults.
OpenMethodNamedExtensionCaller := {}

Rows := {}.{
    wrapped : Rows -> Try(Str, [Unavailable, Missing, ..others])
    wrapped = |_| Ok("x")
}

wrap : Rows -> Try(Str, [Unavailable, Missing, ..others])
wrap = |rows| rows.wrapped()

use : Rows -> Try(Str, [Unavailable, Missing, Other])
use = |rows| {
    s = wrap(rows)?
    Ok(s)
}

expect {
    use(Rows.{}) == Ok("x")
}
