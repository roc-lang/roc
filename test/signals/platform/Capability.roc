import HostValue exposing [HostValue]

Capability(a) := [
	Capability(HostValue.CapabilityHandle),
].{
	new_with_eq : (a, a -> Bool) -> Capability(a)
	new_with_eq = |is_equal| {
		split : Box(a) -> { keep : Box(a), out : Box(a) }
		split = |boxed| {
			value = Box.unbox(boxed)
			{ keep: Box.box(value), out: Box.box(value) }
		}

		split_handle : Box((Box(a) -> { keep : Box(a), out : Box(a) }))
		split_handle = Box.box(split)

		clone : HostValue -> HostValue
		clone = |host_value| {
			boxed : Box(a)
			boxed = HostValue.get_with_split(host_value, split_handle)
			HostValue.store_with_existing_capability(boxed, host_value)
		}

		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : a
			left = Box.unbox(HostValue.get_with_split(left_hv, split_handle))
			right : a
			right = Box.unbox(HostValue.get_with_split(right_hv, split_handle))
			is_equal(left, right)
		}

		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(a)
			boxed = HostValue.take_with_split(host_value, split_handle)
			_ = boxed
			{}
		}

		Capability({ clone: Box.box(clone), eq: Box.box(eq), drop: Box.box(drop) })
	}

	new : {} -> Capability(a)
		where [
			a.is_eq : a, a -> Bool,
		]
	new = |_| Capability.new_with_eq(|left, right| left.is_eq(right))

	handle : Capability(a) -> HostValue.CapabilityHandle
	handle = |Capability(handle_value)| handle_value

	eq : Capability(a) -> Box((HostValue, HostValue -> Bool))
	eq = |Capability(handle_value)| handle_value.eq

	drop : Capability(a) -> Box((HostValue -> {}))
	drop = |Capability(handle_value)| handle_value.drop

	store : Box(a), Capability(a) -> HostValue
	store = |boxed, cap| HostValue.store_with_capability(boxed, Capability.handle(cap))

	get : HostValue, Capability(a) -> Box(a)
	get = |host_value, cap| HostValue.get_with_capability(host_value, Capability.handle(cap))

	take : HostValue, Capability(a) -> Box(a)
	take = |host_value, cap| HostValue.take_with_capability(host_value, Capability.handle(cap))
}
