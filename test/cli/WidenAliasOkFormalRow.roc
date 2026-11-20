# An alias whose formal sits in the `Try`'s OK position. A `where` method's
# rows in the arguments of `Try` are generated as written, and no coercion
# reaches an argument of a named type, so `x.status()` has exactly the ok row
# the where clause writes. The body rebuilds the ok value at the annotated
# result's wider row with `map_ok`.
WidenAliasOkFormalRow := {}

OkRes(a) : Try(a, [IoErr])

describe : a -> OkRes([Red, Green, Blue]) where [a.status : a -> OkRes([Red, Green])]
describe = |x| x.status().map_ok(|color| match color {
    Red => Red
    Green => Green
})

closed_value : OkRes([Red, Green])
closed_value = Ok(Red)

Job := [Pending].{
    status : Job -> OkRes([Red, Green])
    status = |_| closed_value
}

Fallible := [Fallible].{
    status : Fallible -> OkRes([Red, Green])
    status = |_| Err(IoErr)
}

expect describe(Job.Pending) == Ok(Red)
expect describe(Fallible.Fallible) == Err(IoErr)
