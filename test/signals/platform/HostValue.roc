HostValue := [HostValue(U64)].{
	store : Box(a) -> HostValue
	get : HostValue -> Box(a)
	take : HostValue -> Box(a)
	clone : HostValue -> HostValue
}
