ParserTagUnionRuntime :: [].{}

Format := [Default].{
	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_, state|
		match state {
			FieldValue(value) => Ok({ value, rest: Done })
			_ => Err(MissingRequired)
		}

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, _, state|
		match state {
			FieldValue(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done({ rest: state }))
			Tag(_) => Err(MissingRequired)
		}

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, _| Ok(Done)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired

	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [MissingRequired])
	parse_tag_union = |format, spec, state|
		match state {
			Tag(tag_name) => Encoding.ParseTagUnionSpec.parse(spec, {
				tag: tag_name,
				encoding: format,
				state: State.FieldValue("payload"),
				missing: MissingRequired,
			})
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

main : Try([One({ foo : Str }), Two], [MissingRequired])
main = parse("One")

expect main == Ok(One({ foo: "payload" }))
