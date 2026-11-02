# One generic body runs at several type arguments in turn, directly and through
# recursion. A composition reused across type arguments prints the wrong shape.
wrap = |value| { item: value, more: [value] }

show = |value| Str.inspect(wrap(value))

same = |value| wrap(value) == wrap(value)

equal = |a, b| a == b

# The dictionary `equal` receives captures this body's descriptor of `value`.
held_equal = |a, b| equal({ held: a }, { held: b })

# Each level of the recursion runs the body at the type its caller holds.
countdown = |value, n| {
	if n == 0 {
		show(value)
	} else {
		"${show(value)};${countdown(value, n - 1)}"
	}
}

# Polymorphic recursion: every level's composition takes the previous level's
# result as its operand.
deepen : a, U64 -> Str
deepen = |value, n| {
	if n == 0 {
		Str.inspect(value)
	} else {
		"${Str.inspect([value])}/${deepen([value], n - 1)}"
	}
}

# A recursive type's descriptor closes once per type argument.
Chain(a) := [End, Link(a, Chain(a))]

chain = |value, n| {
	if n == 0 {
		Chain.End
	} else {
		Chain.Link(value, chain(value, n - 1))
	}
}

show_chain = |value, n| Str.inspect(chain(value, n))

main! = |_args| {
	numbers : List(U64)
	numbers = [1, 2]
	for n in numbers {
		echo!(show(n))
		echo!(show(Str.inspect(n)))
		echo!(show(n))
		echo!(show({ tag: n }))
		echo!(countdown(n, 1))
		echo!(countdown(Str.inspect(n), 1))
		echo!(if (same(n) and same(Str.inspect(n))) "same" else "bad")
		echo!(Str.inspect(held_equal(n, n)))
		echo!(Str.inspect(held_equal(Str.inspect(n), "other")))
		echo!(Str.inspect(held_equal(n, n + 1)))
		echo!(Str.inspect(held_equal(Str.inspect(n), Str.inspect(n))))
		echo!(deepen(n, 2))
		echo!(deepen(Str.inspect(n), 2))
		echo!(show_chain(n, 2))
		echo!(show_chain(Str.inspect(n), 2))
		echo!(show_chain(n, 1))
	}
	Ok({})
}
