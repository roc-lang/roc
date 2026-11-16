# repro for https://github.com/roc-lang/roc/issues/13010
# A constant holding a recursive closure, which captures itself, and a second
# closure that captures both the recursive closure and the list the recursive
# closure also captures. Its restore binds the recursive closure around its
# own construction, shares the list between the two closures, and reads back
# the values the constant was evaluated to.
Counter : { a : (U64 -> U64), b : (U64 -> U64) }

make : List({ weight : U64, label : Str }) -> Counter
make = |items| {
	total = |n| if (n == 0) List.len(items) else total(n - 1) + n
	labels = |n| Str.count_utf8_bytes(Str.join_with(List.map(items, |item| item.label), ",")) + total(n)
	{ a: total, b: labels }
}

counter : Counter
counter = make([{ weight: 1, label: "one" }, { weight: 2, label: "two" }, { weight: 3, label: "three" }])

main! = |args| {
	n = args.len()
	echo!("${(counter.a)(n).to_str()} ${(counter.b)(n).to_str()}\n")
	Ok({})
}
