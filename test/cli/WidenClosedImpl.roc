# `status`'s published result row is CLOSED (its
# body returns the closed top-level `closed_value`), and the where-method use
# in `describe` requests the wider row `[Ok(Str), Err(Str), Extra]`. The
# implementation stays specialized at its declared row, and its result row
# joins the request's (design.md "Row Joins"). `Extra` sorts between `Err` and
# `Ok`, so a result numbered by the declared row alone shows up as a wrong
# discriminant through `show`, on both backends.
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
