ParserMissingRecordMethod :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_| Err(MissingRequired)

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, state| Ok(state)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired
}

State := [Present(Str)]

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

main : Try({ foo : Str }, [MissingRequired])
main = parse("foo: bar")
