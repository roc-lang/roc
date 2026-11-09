# A method of a type declared in a block, named through a local alias of that
# type, is the same local binding naming it through the type itself is.
initial_count = {
	Counter := [Counter(U64)].{
		unwrap = |Counter(n)| n
	}
	Alias : Counter
	Alias.unwrap(Counter(42.U64))
}

offset_count = |m| {
	Counter := [Counter(U64)].{
		unwrap = |Counter(n)| n
		wrap = |n| Counter(n)
	}
	Alias : Counter
	add = |x| Alias.unwrap(Alias.wrap(x + m))
	add(40)
}

main! = |_args| {
	echo!(initial_count.to_str())
	echo!(offset_count(2).to_str())
	Ok({})
}
