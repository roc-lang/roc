# Closed dictionaries, inside a generic body and out, beside dictionaries that
# capture a body's descriptor of its type variable.
Point := { x : U64 }.{
	is_eq = |a, b| a.x == b.x
}

equal = |a, b| a == b

# Compares through a dictionary of closed types at every type this body runs at.
tagged = |value| {
	same = equal(Point({ x: 1 }), Point({ x: 1 }))
	{ value, same }
}

# The dictionary `equal` receives captures this body's descriptor of `value`.
held_equal = |a, b| equal({ held: a }, { held: b })

main! = |_args| {
	numbers : List(U64)
	numbers = [1, 2]
	for n in numbers {
		echo!(Str.inspect(tagged(n)))
		echo!(Str.inspect(tagged(Str.inspect(n))))
		echo!(Str.inspect(equal(n, n + 1)))
		echo!(Str.inspect(held_equal(n, n)))
		echo!(Str.inspect(held_equal(Str.inspect(n), "other")))
		echo!(Str.inspect(held_equal(n, n + 1)))
		echo!(Str.inspect(held_equal(Str.inspect(n), Str.inspect(n))))
	}
	Ok({})
}
