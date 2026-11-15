ParseTagUnionSpecInList :: [].{}

## A format's unannotated `parse_tag_union` stores its compiler-generated
## `ParseTagUnionSpec` in a list literal and takes it back out with
## `first`. The expectations parse a tag union and a tag union nested in a
## payload.
## The format renames every field and offers the payload record its
## renamed field name, which only the generated spec carries.

same = |x| x

pick = |c, a, b| if (c) a else b

Format := [Default].{
	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_, state|
		match state {
			FieldValue(value) => Ok({ value, rest: Done })
			_ => Err(MissingRequired)
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [MissingRequired])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		[MissingRequired],
	)
	parse_record_field = |_, _, state|
		match state {
			FieldValue(_) => Ok(TryField({ name: "foo!", rest: state }))
			Done => Ok(Done(state))
			Tag(_) => Err(MissingRequired)
		}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [MissingRequired])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, _| Ok(Done)

	missing_field : Format, Str -> [MissingRequired]
	missing_field = |_, _| MissingRequired

	rename_field : Format, Str -> Str
	rename_field = |_, name| name.concat("!")

	parse_tag_union = |format, spec, state|
		match state {
			Tag(tag_name) => {
				specs = [spec, same(spec)]
				Encoding.ParseTagUnionSpec.parse(
					(specs.first() ?? spec),
					{
						tag: tag_name,
						encoding: format,
						state: State.FieldValue("payload"),
						start_payloads: |payload_state, _count| Ok(payload_state),
						next_payload: |payload_state, _index, _count| Ok(payload_state),
						finish_payloads: |payload_state, _count| Ok(payload_state),
						missing: MissingRequired,
					},
				)
			}
			_ => Err(MissingRequired)
		}
}

State := [Tag(Str), FieldValue(Str), Done]

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Tag(input))?
	Ok(parsed.value)
}

expect {
	value : Try([One({ foo : Str }), Two], [MissingRequired])
	value = parse("One")
	value == Ok(One({ foo: "payload" }))
}

expect {
	value : Try([Outer([Inner({ foo : Str }), Other]), Empty], [MissingRequired])
	value = parse("Empty")
	value == Ok(Empty)
}
