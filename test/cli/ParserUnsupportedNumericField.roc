ParserUnsupportedNumericField :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name| name

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

	missing_record_field : Str, State -> [MissingRequired]
	missing_record_field = |_, _| MissingRequired
}

State := [Done]

parse : State -> Try(a, [MissingRequired])
	where [
		a.parser : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser(Format.Default)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

main : Try({ count : I64 }, [MissingRequired])
main = parse(State.Done)
