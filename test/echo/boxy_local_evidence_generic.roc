# A method of a function-body type that captures a local, reached as
# where-clause evidence of generic procedures: directly, forwarded through a
# second generic procedure, from a closure inside the generic procedure, from a
# closure handed to another procedure, and from a closure that escapes the
# block declaring the type.

get_value = |x| x.value()

outer = |x| get_value(x) + get_value(x)

twice = |x| {
	f = || x.value()
	f() + f()
}

apply = |f| f()

mk = |x| || x.value()

make_reader = |n| {
	label = Str.concat("abc", n.to_str())
	Counter := { count : U64 }.{
		value = |c| c.count + Str.count_utf8_bytes(label)
	}

	mk(Counter.{ count: n })
}

main! = |args| {
	extra = List.len(args) + 1
	Counter := { count : U64 }.{
		value = |c| c.count + extra
	}

	c = Counter.{ count: 1 }
	reader = make_reader(10)
	via_closure = apply(|| get_value(Counter.{ count: 5 }))
	echo!("${get_value(c).to_str()} ${outer(c).to_str()} ${twice(c).to_str()} ${reader().to_str()} ${via_closure.to_str()}\n")
	Ok({})
}
