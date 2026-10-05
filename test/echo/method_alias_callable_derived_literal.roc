# A method bound to a generic function whose body converts a numeric literal
# through another requirement's method (`c.count + 1`). The dispatch's
# evidence for that function derives from its callable, including the
# literal's receiver, which only the `plus` target's signature fixes: called
# directly, through a generic caller, through two generic callers, and with a
# function-body nominal and a capturing function.

scale = |c| c.count * 2 + 1

Counter := { count : U64 }.{
	value = scale
}

call_value = |x| x.value()

twice = |x| call_value(x) + call_value(x)

main! = |args| {
	helper = |c| c.count + 1
	Local := { count : U64 }.{
		value = helper
	}

	extra = List.len(args) + 10
	capturing = |c| c.count + extra
	Captured := { count : U64 }.{
		value = capturing
	}

	local = Local.{ count: 4 }
	counter = Counter.{ count: 3 }
	captured = Captured.{ count: 5 }
	echo!("${Str.inspect(local.value())} ${Str.inspect(counter.value())} ${Str.inspect(captured.value())}\n")
	echo!("${Str.inspect(call_value(local))} ${Str.inspect(call_value(counter))} ${Str.inspect(call_value(captured))} ${Str.inspect(twice(local))} ${Str.inspect(twice(counter))}\n")
	Ok({})
}
