import HostValue exposing [HostValue]

Capability(a) := [
	Capability(
		{
			tag : HostValue.TypeTag(a),
			eq : Box((HostValue, HostValue -> Bool)),
			drop : Box((HostValue -> {})),
		},
	),
].{
	new_with_eq : (a, a -> Bool) -> Capability(a)
	new_with_eq = |is_equal| {
		tag = HostValue.new_tag({})

		eq : HostValue, HostValue -> Bool
		eq = |left_hv, right_hv| {
			left : a
			left = Box.unbox(HostValue.get_tagged(left_hv, tag))
			right : a
			right = Box.unbox(HostValue.get_tagged(right_hv, tag))
			is_equal(left, right)
		}

		drop : HostValue -> {}
		drop = |host_value| {
			boxed : Box(a)
			boxed = HostValue.take_tagged(host_value, tag)
			_ = boxed
			{}
		}

		Capability({ tag, eq: Box.box(eq), drop: Box.box(drop) })
	}

	new : {} -> Capability(a)
		where [
			a.is_eq : a, a -> Bool,
		]
	new = |_| Capability.new_with_eq(|left, right| left.is_eq(right))

	tag : Capability(a) -> HostValue.TypeTag(a)
	tag = |Capability(payload)| payload.tag

	eq : Capability(a) -> Box((HostValue, HostValue -> Bool))
	eq = |Capability(payload)| payload.eq

	drop : Capability(a) -> Box((HostValue -> {}))
	drop = |Capability(payload)| payload.drop

	store : Box(a), Capability(a) -> HostValue
	store = |boxed, cap| HostValue.store_tagged(boxed, Capability.tag(cap))

	get : HostValue, Capability(a) -> Box(a)
	get = |host_value, cap| HostValue.get_tagged(host_value, Capability.tag(cap))

	take : HostValue, Capability(a) -> Box(a)
	take = |host_value, cap| HostValue.take_tagged(host_value, Capability.tag(cap))
}
