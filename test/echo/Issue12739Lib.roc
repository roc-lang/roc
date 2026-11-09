Issue12739Lib :: {}.{
	describe : a -> Str
		where [a.label : a -> Str]
	describe = |x| "<${x.label()}>"

	render = describe

	plain : a -> Str
	plain = |_x| "plain"

	same : a, a -> Bool
	same = |_a, _b| Bool.False
}
