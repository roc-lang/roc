Eq := [].{
	same : I64, I64 -> Bool
	# Binding a local keeps this from being a wrapper, which every program
	# inlines at its calls and so no pack offers.
	same = |x, y| {
		matches = x == y
		matches
	}
}
