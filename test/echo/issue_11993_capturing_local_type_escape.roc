# repro for https://github.com/roc-lang/roc/issues/11993
#
# `Counter` is declared in `make`'s body and its `value` method captures
# `offset`, a local of that body, so a `Counter` may not leave the block that
# declares it. `roc check` must report the escape rather than crash, and the
# call outside the block must become a runtime error.

app [main!] {}

make = |offset, n| {
	Counter := { count : U64 }.{
		value = |counter| counter.count + offset
	}
	Counter.{ count: n }
}

main! = |args| {
	c = make(1, args.len())
	echo!("${c.value().to_str()}\n")
	Ok({})
}
