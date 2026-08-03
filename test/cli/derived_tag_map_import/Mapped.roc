Mapped :: {}.{
	Maybe(a) := [Just(a), Nothing].{
		map : _
	}

	OpaqueMaybe(a) :: [OpaqueJust(a), OpaqueNothing].{
		map : _
	}

	opaque_just : a -> OpaqueMaybe(a)
	opaque_just = |value| OpaqueJust(value)

	opaque_value : OpaqueMaybe(Str) -> Str
	opaque_value = |maybe| match maybe {
		OpaqueJust(value) => value
		OpaqueNothing => "nothing"
	}
}
