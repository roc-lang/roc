ShownMaker := [].{
	make = |id| {
		r = { id: id }
		(Str.inspect(r), r)
	}

	describe = |c| {
		n = match c {
			Red => 1
			Green => 2
		}
		(Str.inspect(c), n)
	}
}
