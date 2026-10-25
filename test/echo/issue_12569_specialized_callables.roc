# CTFE must freeze the captured string and the callable without Boxy metadata.
make_formatter : Str -> (I64 -> Str)
make_formatter = |prefix| |n| "${prefix}${n.to_str()}"

stored = make_formatter("a captured string longer than inline storage: ")

# Returning a captured callable exercises the closure-reuse ownership boundary.
make_adder : I64 -> (I64 -> I64)
make_adder = |amount| |value| amount + value

# Every recursive step crosses an explicitly erased callable boundary.
countdown : I64 -> I64
countdown = |n| if n == 0 {
	42
} else {
	step = Box.unbox(Box.box(countdown))
	step(n - 1)
}

main! = |args| {
	amount : I64
	amount = List.len(args).to_i64_wrap() + 7
	factory = Box.unbox(Box.box(make_adder))
	add = Box.unbox(Box.box(factory(amount)))
	echo!("${stored(add(1))}\n")
	echo!("${countdown(20000 + amount).to_str()}\n")
	Ok({})
}
