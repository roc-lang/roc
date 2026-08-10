ParserTagUnionRuntime :: [].{}

Format := [Default].{
	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired, ..])
	parse_str = |_, state|
		match state {
			FieldValue(value) => Ok({ value, rest: Done })
			_ => Err(MissingRequired)
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [MissingRequired, ..])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		[MissingRequired, ..],
	)
	parse_record_field = |_, _, state|
		match state {
			FieldValue(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done(state))
			Tag(_) => Err(MissingRequired)
		}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [MissingRequired, ..])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [MissingRequired, ..])
	skip_record_field = |_, _| Ok(Done)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired

	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [MissingRequired, ..])
	parse_tag_union = |format, spec, state|
		match state {
			Tag(tag_name) => Encoding.ParseTagUnionSpec.parse(spec, {
				tag: tag_name,
				encoding: format,
				state: State.FieldValue("payload"),
				start_payloads: |payload_state, _count| Ok(payload_state),
				next_payload: |payload_state, _index, _count| Ok(payload_state),
				finish_payloads: |payload_state, _count| Ok(payload_state),
				missing: MissingRequired,
			})
			_ => Err(MissingRequired)
		}
}

State := [Tag(Str), FieldValue(Str), Done]

parse : Str -> Try(a, [MissingRequired, MissingRequiredField(Str)])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired, MissingRequiredField(Str)])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Tag(input))?
	Ok(parsed.value)
}

main : Try([One({ foo : Str }), Two], [MissingRequired, MissingRequiredField(Str)])
main = parse("One")

expect main == Ok(One({ foo: "payload" }))
