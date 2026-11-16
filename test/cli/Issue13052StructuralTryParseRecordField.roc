Issue13052StructuralTryParseRecordField :: [].{}

## A format whose `parse_record_field` has no annotation builds its result
## from `Ok(...)` and `Err(...)` tag literals, so its inferred result type is
## an anonymous tag union. The derived record parser's use lifts that union
## into `Try`, and specializing the method at the compiler-generated
## `FieldNames`/`FieldName` representation replays the lift. The expectation
## decodes `{ x : 7 }`.

Error : [Bad(U64), MissingRequiredField(Str)]

Inner := [Default].{
	parse_record_field : Inner,
	Encoding.FieldName.FieldNames(_shape),
	U64 -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : U64 }),
			TryField({ name : Str, rest : U64 }),
			TryFieldCaseless({ name : Str, rest : U64 }),
			Continue(U64),
			Done(U64),
		],
		Error,
	)
	parse_record_field = |_, fields, pos| {
		for f in fields.iter() {
			return Ok(Field({ field: f, rest: pos }))
		}
		Err(Bad(pos))
	}
}

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

	parse_record_field = |_, names, state|
		match Inner.parse_record_field(Inner.Default, names, state.pos) {
			Ok(Field({ field, rest })) => Ok(Field({ field, rest: { ..state, pos: rest } }))
			Ok(TryField({ name, rest })) => Ok(TryField({ name, rest: { ..state, pos: rest } }))
			Ok(TryFieldCaseless({ name, rest })) => Ok(TryFieldCaseless({ name, rest: { ..state, pos: rest } }))
			Ok(Continue(pos)) => Ok(Continue({ ..state, pos }))
			Ok(Done(pos)) => Ok(Done({ ..state, pos }))
			Err(err) => Err(err)
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
