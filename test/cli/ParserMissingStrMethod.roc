ParserMissingStrMethod :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

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
	parse_record_field = |_, _, state| Ok(Done({ rest: state }))

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, state| Ok(state)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired

	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [MissingRequired])
	parse_tag_union = |_, _, _| Err(MissingRequired)
}

State := [Present(Str)]

parse : Str -> Try(a, [MissingRequired]) where [
	a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

main : Try({ aaa : Str, choice : [One(Str)] }, [MissingRequired])
main = parse("One")
