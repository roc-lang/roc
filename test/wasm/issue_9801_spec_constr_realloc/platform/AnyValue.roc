AnyValue := [AnyValue(U64)].{
	TypeTag(a) := [TypeTag(U64)]

	new_tag : {} -> Box(TypeTag(a))
	new_tag = |_| Box.box(TypeTag(0))

	store_tagged : Box(a), Box(TypeTag(a)) -> AnyValue
	get_tagged : AnyValue, Box(TypeTag(a)) -> Box(a)
	take : AnyValue -> Box(a)
	clone : AnyValue -> AnyValue
}
