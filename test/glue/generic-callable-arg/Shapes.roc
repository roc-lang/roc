Shapes := [].{
	Handler(a) := [H(Box({ y : a } -> {}))]
	Step := [Done, Emit(U64)]
	Adder := Box(U64 -> U64)
}
