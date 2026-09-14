# polarity_phase_two.md W6b: `status`'s published result row is CLOSED (its
# body returns the closed top-level `closed_value`), and the where-method use
# in `describe` requests the wider row `[Ok(Str), Err(Str), Extra]`. The
# implementation must stay specialized at its declared row and be reached
# through a generated adapter that re-tags into the request. `Extra` sorts
# between `Err` and `Ok`, so a wrong re-tag shows up as a wrong discriminant
# through `show`, on both backends.
WidenClosedImpl := {}

describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| x.status()

closed_value : [Ok(Str), Err(Str)]
closed_value = Ok("cv")

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
