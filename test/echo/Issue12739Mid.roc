import Issue12739Lib

Issue12739Mid :: {}.{
	Thing := { n : U8 }.{
		label = |t| "thing ${t.n.to_str()}"
		to_inspect = Issue12739Lib.render
		is_eq = Issue12739Lib.same
	}

	make : U8 -> Thing
	make = |n| Thing.({ n: n })
}
