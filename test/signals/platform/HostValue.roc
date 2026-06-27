HostValue := [HostValue(U64)].{
	CapabilityHandle := {
		clone : Box((HostValue -> HostValue)),
		drop : Box((HostValue -> {})),
		eq : Box((HostValue, HostValue -> Bool)),
	}

	TextReadHandle := {
		capability : CapabilityHandle,
		read : Box((HostValue -> Str)),
	}

	BoolReadHandle := {
		capability : CapabilityHandle,
		read : Box((HostValue -> Bool)),
	}

	TaskRequestReadHandle := {
		capability : CapabilityHandle,
		read : Box((HostValue -> Str)),
	}

	EventReducerHandle := {
		capability : CapabilityHandle,
		transform : Box((HostValue, HostValue -> HostValue)),
	}

	clone : HostValue -> HostValue
	store_with_capability : Box(a), CapabilityHandle -> HostValue
	store_with_existing_capability : Box(a), HostValue -> HostValue
	get_with_capability : HostValue, CapabilityHandle -> Box(a)
	take_with_capability : HostValue, CapabilityHandle -> Box(a)

	get_with_split : HostValue, Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> Box(a)
	take_with_split : HostValue, Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> Box(a)
}
