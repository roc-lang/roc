Issue13052DispatchedFieldNamesParseRecordField :: [].{}

## A format's `parse_record_field` leaves its field-names parameter
## unannotated and only calls `.iter()` on it, so that parameter's checked
## type is a variable the derived record parser's use resolves to
## `Encoding.FieldName.FieldNames`. Specializing the method at the
## compiler-generated `FieldNames`/`FieldName` representation binds the
## parameter to that representation. The expectation decodes `{ x : 7 }`.

Error : [Bad(U64), MissingRequiredField(Str)]

bad : U64 -> Error
bad = |pos| Bad(pos)

State : { pos : U64, bytes : List(U8) }

Format := [Default].{
	missing_field : Format, Str -> Error
	missing_field = |_, name| MissingRequiredField(name)

	rename_field = |_, name| name

	parse_u64 : Format, State -> Try({ value : U64, rest : State }, Error)
	parse_u64 = |_, state|
		match state.bytes.get(state.pos) {
			Ok(b) => Ok({ value: b.to_u64(), rest: { ..state, pos: state.pos + 1 } })
			Err(_) => Err(Bad(state.pos))
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], Error)
	parse_record_start = |_, state| Ok(Counted({ len: 1, rest: state }))

	parse_record_field : Format, _, State -> Try(_, Error)
	parse_record_field = |_, names, state| {
		for f in names.iter() {
			return Ok(Field({ field: f, rest: state }))
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
	value : Try({ x : U64 }, Error)
	value = decode([7])
	value == Ok({ x: 7 })
}
