ParserMissingTagUnionMethod :: [].{}

Format := [Default].{
	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_| Err(MissingRequired)
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

main : Try([One], [MissingRequired])
main = parse("One")
