Issue13052FieldNamesRenamedInTryResult :: [].{}

## A module-level helper returns the result of `FieldNames.rename_fields`
## inside a `Try`, and a format's `parse_record_field` matches it out and
## iterates it. The format renames
## every field, and a field only matches under its renamed name, so the names
## read back are the generated ones. The expectations decode a record and a
## nested record.

Error : [Bad(U64), MissingRequiredField(Str)]

bad : U64 -> Error
bad = |pos| Bad(pos)

State : { pos : U64, bytes : List(U8) }

same = |x| x

again : Encoding.FieldName.FieldNames(shape) -> Try(Encoding.FieldName.FieldNames(shape), [Never])
again = |held| Ok(Encoding.FieldName.FieldNames.rename_fields(held, |name| name.concat("?")))

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

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try([Field({ field : Encoding.FieldName(_shape), rest : State }), TryField({ name : Str, rest : State }), TryFieldCaseless({ name : Str, rest : State }), Continue(State), Done(State)], Error)
	parse_record_field = |_, names, state| {
		idx = (state.bytes.get(state.pos) ?? 255).to_u64()
		var $i = 0
		renamed = match again(names) {
			Ok(r) => r
			Err(_) => names
		}
		for f in renamed.iter() {
			if $i == idx and f.name().ends_with("!?") {
				return Ok(Field({ field: f, rest: { ..state, pos: state.pos + 1 } }))
			}
			$i = $i + 1
		}
		Err(bad(state.pos))
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
