# `status`'s published result row is CLOSED (its
# body returns the closed top-level `closed_value`), and the where-method use
# in `describe` requests the wider row `[Ok(Str), Err(Str), Extra]`. The
# implementation must stay specialized at its declared row and be reached
# through a generated adapter that re-tags into the request. `Extra` sorts
# between `Err` and `Ok`, so a wrong re-tag shows up as a wrong discriminant
# through `show`, on both backends.
WidenClosedImpl := {}

describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| x.status()

# `closed_value` is deliberately UNANNOTATED, and its row is read out of a nominal
# field. Neither an annotation nor a forwarding function can produce a closed
# row any more: an annotated value's implicitly opened row is quantified
# (design.md "Polarity"), and a top-level function that FORWARDS a closed value
# has its result row coerced open again at every use (design.md "Row
# Subsumption"). A nominal declaration's body closes its rows as written, so a
# field of `Closed` is a closed source that no coercion reopens, and `closed_value`
# is a top-level constant whose row is closed, which is what this fixture needs.
Closed := { v : [Ok(Str), Err(Str)] }

closed_value = Closed.{ v: Ok("cv") }.v

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
