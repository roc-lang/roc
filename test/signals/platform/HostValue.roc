HostValue := [HostValue(U64)].{
	TypeTag(a) := [TypeTag(U64)]

	new_tag : {} -> Box(TypeTag(a))
	new_tag = |_| Box.box(TypeTag(0))
	new_unit_payload_tag : {} -> Box(TypeTag({}))
	new_unit_payload_tag = |_| Box.box(TypeTag(1))
	new_str_payload_tag : {} -> Box(TypeTag(Str))
	new_str_payload_tag = |_| Box.box(TypeTag(2))
	new_bool_payload_tag : {} -> Box(TypeTag(Bool))
	new_bool_payload_tag = |_| Box.box(TypeTag(3))

	store : Box(a) -> HostValue
	store_tagged : Box(a), Box(tag) -> HostValue
	get : HostValue -> Box(a)
	get_tagged : HostValue, Box(TypeTag(a)) -> Box(a)
	take : HostValue -> Box(a)
	take_tagged : HostValue, Box(TypeTag(a)) -> Box(a)
	clone : HostValue -> HostValue
}
