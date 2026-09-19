# The widening dispatch sits inside a generalized
# local (`widen`) whose scheme quantifies the row tail, so the request reaches
# template completion through the local's instantiation rather than from the
# enclosing definition's annotation. The implementation's row is still closed
# by `closed_value`, so the adapter is still the only way to reach it.
WidenClosedImplLocalScope := {}

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

describe : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| {
    widen = |y| y.status()
    show(widen(x))
}

expect describe(Job.Pending) == "Ok(cv)"
