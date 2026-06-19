ParserOptionalOnlyRecordNoMissingMethod :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name| name

	parse_str : State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_| Err(MissingRequired)

	parse_record_field : Fields(_shape), State -> Try(
		[
			Field({ field : Field(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, state| Ok(Done({ rest: state }))

	skip_record_field : State -> Try(State, [MissingRequired])
	skip_record_field = |state| Ok(state)

	missing_optional_field : Str, State -> [Missing]
	missing_optional_field = |_, _| Missing
}

State := [Present(Str)]

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parser : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

main : Try({ foo : Try(Str, [Missing]) }, [MissingRequired])
main = parse("")
