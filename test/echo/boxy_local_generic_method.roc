# A method of a function-body type that captures a local and is generic in
# its other argument, called by dispatch and by its qualified name.

main! = |args| {
	bump = List.len(args) + 100
	Step := { by : U64 }.{
		apply = |s, x| x + s.by + bump
	}

	step = Step.{ by: 2 }
	a = step.apply(5)
	b = Step.apply(step, 7)
	echo!("${Str.inspect(a)} ${Str.inspect(b)}\n")
	Ok({})
}
