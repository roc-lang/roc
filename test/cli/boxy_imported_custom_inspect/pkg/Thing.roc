Thing := [Wrapped(U64)].{
	new : U64 -> Thing
	new = |value| Wrapped(value)

	to_inspect : Thing -> Str
	to_inspect = |_| "imported custom inspect"
}
