RemainderDivisorFactScope := {}

# A remainder inside a `List.map` closure proves its divisor nonzero only on
# paths that run the remainder. Mapping an empty list never runs it, so
# `n == 0` stays true after the map.
is_empty_len : List(U64) -> Bool
is_empty_len = |xs| {
	n = xs.len()
	_ys = xs.map(|i| i % n)
	n == 0
}

expect is_empty_len([])

expect !is_empty_len([3, 4])
