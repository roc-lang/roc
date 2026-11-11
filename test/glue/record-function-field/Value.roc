Value :: { first : Box(U64 -> U64) }.{
	value : Value
	value = { first: Box.box(|x| x) }
}
