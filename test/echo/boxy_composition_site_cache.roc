# One generic body alternates instantiations, with repeated calls at each type.
pair = |value| { first: value, second: [value] }

same = |value| pair(value) == pair(value)

main! = |_args| {
	numbers : List(U64)
	numbers = [1, 2, 3, 4]
	for n in numbers {
		echo!(if (same(n)) "n" else "bad")
		echo!(if (same(n)) "n" else "bad")
		echo!(if (same(Str.inspect(n))) "s" else "bad")
		echo!(if (same(Str.inspect(n))) "s" else "bad")
		echo!(if (same(n)) "n" else "bad")
	}
	Ok({})
}
