# An alias whose formal sits in the `Try`'s OK position. Per-use opening is
# withheld from that position, so `x.status()` has the closed ok row the where
# clause writes. The ok row is within the row coercion's reach (a tag payload
# of `Try`'s backing), so the body's value relates to the annotated result
# through a widening copy and crosses into it through a row coercion
# (design.md "Row Subsumption"): the body use widens the ok row rather than
# rejecting it, and the two ok rows are one row on every backend (design.md
# "Row Joins").
WidenAliasOkFormalRow := {}

OkRes(a) : Try(a, [IoErr])

describe : a -> OkRes([Red, Green, Blue]) where [a.status : a -> OkRes([Red, Green])]
describe = |x| x.status()

closed_value : OkRes([Red, Green])
closed_value = Ok(Red)

Job := [Pending].{
    status : Job -> OkRes([Red, Green])
    status = |_| closed_value
}

expect describe(Job.Pending) == Ok(Red)
