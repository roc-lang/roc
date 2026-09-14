# polarity_phase_two.md W6b: the widening dispatch sits inside a generalized
# local (`widen`) whose scheme quantifies the row tail, so the request reaches
# template completion through the local's instantiation rather than from the
# enclosing definition's annotation. The implementation's row is still closed
# by `closed_value`, so the adapter is still the only way to reach it.
WidenClosedImplLocalScope := {}

closed_value : [Ok(Str), Err(Str)]
closed_value = Ok("cv")

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
