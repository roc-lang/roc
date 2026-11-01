Flow := [Done(U64), Later(Box(U64 -> U64))].{
	done : U64 -> Flow
	done = |value| Done(value)

	later : (U64 -> U64) -> Flow
	later = |step| Later(Box.box(step))

	## A closed function whose type holds a boxed function and whose body
	## neither boxes nor unboxes one.
	score : Flow -> U64
	score = |flow| match flow {
		Done(value) => value + 1
		Later(_) => 0
	}

	resume : Flow, U64 -> U64
	resume = |flow, input| match flow {
		Done(value) => value + input
		Later(boxed) => (Box.unbox(boxed))(input)
	}
}
