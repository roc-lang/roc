HostValue := [HostValue(U64)].{
	CapabilityHandle := {
		clone : Box((HostValue -> HostValue)),
		eq : Box((HostValue, HostValue -> Bool)),
		drop : Box((HostValue -> {})),
	}

	clone : HostValue -> HostValue
	store_with_capability : Box(a), CapabilityHandle -> HostValue
	store_with_existing_capability : Box(a), HostValue -> HostValue
	get_with_capability : HostValue, CapabilityHandle -> Box(a)
	take_with_capability : HostValue, CapabilityHandle -> Box(a)

	get_with_split : HostValue, Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> Box(a)
	take_with_split : HostValue, Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> Box(a)
}
