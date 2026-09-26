# The widening dispatch sits inside a generalized
# local (`widen`) whose scheme quantifies the row tail, so the request reaches
# template completion through the local's instantiation rather than from the
# enclosing definition's annotation. The implementation's row is still closed
# by `closed_value`, so the adapter is still the only way to reach it.
WidenClosedImplLocalScope := {}

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

describe : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| {
    widen = |y| y.status()
    show(widen(x))
}

expect describe(Job.Pending) == "Ok(cv)"
