# polarity_phase_two.md W6b: an implementation that returns its closed-row
# input is still adapted at the wider where-method body request.
describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]]
describe = |x| x.status(Ok("arg"))

Job := [Pending].{
	status : Job, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]
	status = |_, v| v
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(arg)"
