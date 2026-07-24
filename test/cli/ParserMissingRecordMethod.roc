ParserMissingRecordMethod :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError, ..])
	parse_str = |_| Err(FormatError)

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, state| Ok(state)
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

main : Try({ foo : Str }, [FormatError, MissingRequiredField(Str)])
main = parse("foo: bar")
