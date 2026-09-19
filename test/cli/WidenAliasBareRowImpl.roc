# A zero-arity alias whose whole body is a bare row,
# referenced as a where-method's direct result. The reference is a `.lookup`,
# so the instantiator decides the row: `stepAlias` carries `.result` into the
# backing, `stepTagUnion` carries it into the extension, and the marker stays a
# deferred rigid. Nothing in the corpus covered this path, so it is pinned here
# as a regression guard alongside the alias type-argument case.
Status : [Ok(Str), Err(Str)]

describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> Status]
describe = |x| x.status()

# `closed_value` is deliberately UNANNOTATED. An annotated value's implicitly
# opened row is quantified now (design.md "Polarity"), so an annotation can no
# longer produce a closed row at all. `closed` takes the row in an INPUT
# position, where it is generated as written, and returns it, so its result row
# is bound to `[]` by its own body: an input-position parameter is one of the
# closed sources design.md names. `closed_value` is therefore still a top-level
# constant whose row is closed, which is what this fixture needs.
closed : Status -> Status
closed = |v| v

closed_value = closed(Ok("cv"))

Job := [Pending].{
    status : Job -> Status
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
