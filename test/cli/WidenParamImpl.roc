# The result-row widening adapter where the implementation's row is closed by
# a parameter rather than by a top-level constant—an input position stays
# closed as written, and returning it publishes a closed result row. The
# request still lists `Extra`, which sorts between `Err` and `Ok`.
WidenParamImpl := {}

describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]]
describe = |x| x.status(Ok("arg"))

Job := [Pending].{
    status : Job, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]
    status = |_, v| v
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(arg)"
