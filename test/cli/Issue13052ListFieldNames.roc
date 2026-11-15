Issue13052ListFieldNames :: [].{}

## A format's unannotated `parse_record_field` stores its field names, and
## each field it finds, in a list literal and takes them back out with
## `first`. The list item type is the codec contract's public `FieldNames` or
## `FieldName` type, which denotes the compiler-generated representation the
## values carry. The expectations decode a record and a nested record.
## The format renames every field, and a field only matches under its
## renamed name, so the names read back are the generated ones.

Error : [Bad(U64), MissingRequiredField(Str)]

bad : U64 -> Error
bad = |pos| Bad(pos)

State : { pos : U64, bytes : List(U8) }

same = |x| x

find_in = |ns, idx| {
	var $i = 0
	for f in ns.iter() {
		if $i == idx and f.name().ends_with("!") {
			return Ok(f)
		}
		$i = $i + 1
	}
	Err(NotFound)
}

Format := [Default].{
	missing_field : Format, Str -> Error
	missing_field = |_, name| MissingRequiredField(name)

	rename_field = |_, name| name.concat("!")

	parse_u64 : Format, State -> Try({ value : U64, rest : State }, Error)
	parse_u64 = |_, state|
		match state.bytes.get(state.pos) {
			Ok(b) => Ok({ value: b.to_u64(), rest: { ..state, pos: state.pos + 1 } })
			Err(_) => Err(Bad(state.pos))
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], Error)
	parse_record_start = |_, state|
		match state.bytes.get(state.pos) {
			Ok(n) => Ok(Counted({ len: n.to_u64(), rest: { ..state, pos: state.pos + 1 } }))
			Err(_) => Err(Bad(state.pos))
		}

	parse_record_field : Format, _, State -> Try(_, Error)
	parse_record_field = |_, names, state| {
		idx = (state.bytes.get(state.pos) ?? 255).to_u64()
		match find_in([names, same(names)].first() ?? names, idx) {
			Ok(f) => Ok(Field({ field: ([f, same(f)].first() ?? f), rest: { ..state, pos: state.pos + 1 } }))
			Err(NotFound) => Err(bad(state.pos))
		}
	}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], Error)
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, Error)
	skip_record_field = |_, state| Ok(state)
}

decode : List(U8) -> Try(a, Error) where [a.parser_for : Format -> (State -> Try({ value : a, rest : State }, Error))]
decode = |bytes| {
	Value : a
	parsed = Value.parser_for(Format.Default)({ pos: 0, bytes })?
	Ok(parsed.value)
}

expect {
	value : Try({ p : U64 }, Error)
	value = decode([1, 0, 3])
	value == Ok({ p: 3 })
}

expect {
	value : Try({ x : U64, y : { z : U64, w : U64 } }, Error)
	value = decode([2, 0, 7, 1, 2, 1, 5, 0, 5])
	value == Ok({ x: 7, y: { z: 5, w: 5 } })
}
