# polarity_phase_two.md W3: the dispatch target is the enclosing template
# itself. The plan's callable shares its row tail with the template's own
# return row; specializing it against the target clones that plan-side tail
# (identity against identity records no substitution, and the target-side
# clone is discarded), and the classification maps the clone back to the
# template root through the store's identity origins, so the recursive plan
# is `direct_parametric`. The substitution direction itself is pinned by the
# checked_artifact.zig unit test "direct dispatch classification follows
# instantiation clones to the scheme that quantifies them", not here.
OpenMethodRecursiveMethod := {}

Rows := { n : U64 }.{
    wrapped : Rows -> Try(Str, [Unavailable])
    wrapped = |rows| if rows.n == 0 Ok("x") else Rows.{ n: rows.n - 1 }.wrapped()
}

use : Rows -> Try(Str, [Unavailable, Other])
use = |rows| {
    s = rows.wrapped()?
    Ok(s)
}

expect {
    use(Rows.{ n: 2 }) == Ok("x")
}
