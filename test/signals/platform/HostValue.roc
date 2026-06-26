HostValue := [HostValue(U64)].{
	CapabilityHandle(a) := {
		split : Box((Box(a) -> { keep : Box(a), out : Box(a) })),
		eq : Box((HostValue, HostValue -> Bool)),
		drop : Box((HostValue -> {})),
	}

	clone : HostValue -> HostValue
	store_with_capability : Box(a), CapabilityHandle(a) -> HostValue
	get_with_capability : HostValue, CapabilityHandle(a) -> Box(a)
	take_with_capability : HostValue, CapabilityHandle(a) -> Box(a)

	get_with_split : HostValue, Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> Box(a)
	take_with_split : HostValue, Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> Box(a)
}
