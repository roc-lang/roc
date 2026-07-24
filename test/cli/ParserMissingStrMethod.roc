ParserMissingStrMethod :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[FormatError, ..],
	)
	parse_record_field = |_, _, state| Ok(Done({ rest: state }))

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, state| Ok(state)

	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [FormatError, ..])
	parse_tag_union = |_, _, _| Err(FormatError)
}

State := [Present(Str)]

parse : Str -> Try(a, [FormatError, ..errs])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

main : Try({ aaa : Str, choice : [One(Str)] }, [FormatError, MissingRequiredField(Str)])
main = parse("One")
