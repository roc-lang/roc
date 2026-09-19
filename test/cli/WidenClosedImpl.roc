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

# `closed_value` is deliberately UNANNOTATED. An annotated value's implicitly
# opened row is quantified now (design.md "Polarity"), so an annotation can no
# longer produce a closed row at all. `closed` takes the row in an INPUT
# position, where it is generated as written, and returns it, so its result row
# is bound to `[]` by its own body: an input-position parameter is one of the
# closed sources design.md names. `closed_value` is therefore still a top-level
# constant whose row is closed, which is what this fixture needs.
closed : [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]
closed = |v| v

closed_value = closed(Ok("cv"))

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
