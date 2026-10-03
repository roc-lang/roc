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

# `seal` forwards its closed input, which closes its output row, and so the
# row of every value built from it (design.md "Deferred: Row Subsumption").
# This depends on that known limitation (forwarding closes the row): once
# row subsumption lands, this fixture must close its impl row another way.
seal : [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]
seal = |v| v

closed_value : [Ok(Str), Err(Str)]
closed_value = seal(Ok("cv"))

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
