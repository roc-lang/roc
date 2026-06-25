Rb(a) := { value : a }.{
	map2 : Rb(a), Rb(b), (a, b -> c) -> Rb(c)
	map2 = |left, right, combine| {
		value: combine(left.value, right.value),
	}

	field : a -> Rb(a)
	field = |value| { value: value }

	run : Rb(a) -> a
	run = |builder| builder.value
}
