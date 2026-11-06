# repro for https://github.com/roc-lang/roc/issues/12705
# A generic method, called with method syntax at two types, returns an inline
# lambda that calls a local closure capturing a method parameter. Each call
# returns a function that echoes its argument, so the program prints "x" twice.
G := {}.{
	f : G, a -> (Str -> Str)
	f = |_g, x| {
		h = |p| {
			_ = x
			p
		}
		|p| h(p)
	}
}

main! = |_args| {
	echo!(G.({}).f(1.U8)("x"))
	echo!(G.({}).f("s")("x"))
	Ok({})
}
