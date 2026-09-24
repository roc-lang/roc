# A zero-arity alias whose whole body is a bare row,
# referenced as a where-method's direct result. The reference is a `.lookup`,
# so the instantiator decides the row: `stepAlias` carries `.result` into the
# backing, `stepTagUnion` carries it into the extension, and the marker stays a
# deferred rigid. Nothing in the corpus covered this path, so it is pinned here
# as a regression guard alongside the alias type-argument case.
Status : [Ok(Str), Err(Str)]

describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> Status]
describe = |x| x.status()

# `closed_value` is deliberately UNANNOTATED, and its row is read out of a nominal
# field. Neither an annotation nor a forwarding function can produce a closed
# row any more: an annotated value's implicitly opened row is quantified
# (design.md "Polarity"), and a top-level function that FORWARDS a closed value
# has its result row coerced open again at every use (design.md "Row
# Subsumption"). A nominal declaration's body closes its rows as written, so a
# field of `Closed` is a closed source that no coercion reopens, and `closed_value`
# is a top-level constant whose row is closed, which is what this fixture needs.
Closed := { v : Status }

closed_value = Closed.{ v: Ok("cv") }.v

Job := [Pending].{
    status : Job -> Status
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
