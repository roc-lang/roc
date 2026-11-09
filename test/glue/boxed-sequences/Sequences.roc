Sequences := [].{
	Wrapped(a) := { inner : Box(a) }

	hold_iter! : Box(Iter(U8)) => {}
	hold_stream! : Box(Stream(U8)) => {}
}
