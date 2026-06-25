HostValue := [HostValue(U64)].{
	TypeTag(a) := [
		TypeTag(
			{
				id : U64,
				split : Box((Box(a) -> { keep : Box(a), out : Box(a) })),
			},
		),
	]

	split_box : Box(a) -> { keep : Box(a), out : Box(a) }
	split_box = |boxed| {
		value = Box.unbox(boxed)
		{ keep: Box.box(value), out: Box.box(value) }
	}

	new_tag : {} -> TypeTag(a)
	new_tag = |_| {
		split : Box(a) -> { keep : Box(a), out : Box(a) }
		split = |boxed| {
			value = Box.unbox(boxed)
			{ keep: Box.box(value), out: Box.box(value) }
		}
		TypeTag({ id: 0, split: Box.box(split) })
	}
	new_unit_payload_tag : {} -> TypeTag({})
	new_unit_payload_tag = |_| {
		split : Box({}) -> { keep : Box({}), out : Box({}) }
		split = |boxed| {
			value = Box.unbox(boxed)
			{ keep: Box.box(value), out: Box.box(value) }
		}
		TypeTag({ id: 1, split: Box.box(split) })
	}
	new_str_payload_tag : {} -> TypeTag(Str)
	new_str_payload_tag = |_| {
		split : Box(Str) -> { keep : Box(Str), out : Box(Str) }
		split = |boxed| {
			value = Box.unbox(boxed)
			{ keep: Box.box(value), out: Box.box(value) }
		}
		TypeTag({ id: 2, split: Box.box(split) })
	}
	new_bool_payload_tag : {} -> TypeTag(Bool)
	new_bool_payload_tag = |_| {
		split : Box(Bool) -> { keep : Box(Bool), out : Box(Bool) }
		split = |boxed| {
			value = Box.unbox(boxed)
			{ keep: Box.box(value), out: Box.box(value) }
		}
		TypeTag({ id: 3, split: Box.box(split) })
	}
	from_split : Box((Box(a) -> { keep : Box(a), out : Box(a) })) -> TypeTag(a)
	from_split = |split| TypeTag({ id: 0, split })

	store : Box(a) -> HostValue
	store_tagged : Box(a), TypeTag(a) -> HostValue
	get : HostValue -> Box(a)
	get_tagged : HostValue, TypeTag(a) -> Box(a)
	take : HostValue -> Box(a)
	take_tagged : HostValue, TypeTag(a) -> Box(a)
	clone : HostValue -> HostValue
}
