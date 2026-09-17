# A zero-arity alias whose whole body is a bare row,
# referenced as a where-method's direct result. The reference is a `.lookup`,
# so the instantiator decides the row: `stepAlias` carries `.result` into the
# backing, `stepTagUnion` carries it into the extension, and the marker stays a
# deferred rigid. Nothing in the corpus covered this path, so it is pinned here
# as a regression guard alongside the alias type-argument case.
Status : [Ok(Str), Err(Str)]

describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> Status]
describe = |x| x.status()

closed_value : Status
closed_value = Ok("cv")

Job := [Pending].{
    status : Job -> Status
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
