# repro for https://github.com/roc-lang/roc/issues/11993
#
# Methods of a type declared in a function body that capture nothing, even
# when annotated with that type and calling each other, are promoted, so the
# type may leave the block that declares it and its methods work anywhere.

make = |n| {
	Counter := { count : U64 }.{
		value : Counter -> U64
		value = |counter| counter.helper() + Counter.helper(counter)
		helper : Counter -> U64
		helper = |counter| counter.count + 1
		same : Counter, Counter -> Bool
		same = |a, b| a.count == b.count
	}
	Counter.{ count: n }
}

main! = |args| {
	c = make(args.len() + 2)
	echo!("${c.value().to_str()} ${Str.inspect(c.same(c))}\n")
	Ok({})
}
