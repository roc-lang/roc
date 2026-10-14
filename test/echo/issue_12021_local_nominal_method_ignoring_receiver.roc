# A nominal type declared in a function body, whose method ignores its
# receiver and returns a numeral that defaults to `Dec`.
main! = |_args| {
	Counter := { count : U64 }.{
		value = |_| 0
	}

	counter = Counter.{ count: 0 }
	count = counter.value()
	echo!("${count.to_str()}\n")
	Ok({})
}
