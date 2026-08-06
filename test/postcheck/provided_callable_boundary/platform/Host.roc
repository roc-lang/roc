Host := [].{
	make! : U64 => Box(U64 -> U64)

	drop! : Box(U64 -> U64) => {}
}
