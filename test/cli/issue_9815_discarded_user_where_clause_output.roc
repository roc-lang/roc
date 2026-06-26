Thing := [Thing]

Sink := [Sink].{
	from_thing : Thing -> Sink
	from_thing = |_| Sink
}

make : Thing -> output where [output.from_thing : Thing -> output]
make = |thing| {
	Output : output
	Output.from_thing(thing)
}

main! = |_args| {
	_ = make(Thing)

	Ok({})
}
