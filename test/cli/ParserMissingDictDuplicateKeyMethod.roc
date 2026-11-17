ParserMissingDictDuplicateKeyMethod :: [].{}

Format := [Default].{
	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError])
	parse_str = |_, _| Err(FormatError)

	parse_dict_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError])
	parse_dict_start = |_, state| Ok(Uncounted(state))

	parse_dict_next : Format, State -> Try([Entry(State), Done(State)], [FormatError])
	parse_dict_next = |_, state| Ok(Done(state))

	parse_dict_after_key : Format, State -> Try(State, [FormatError])
	parse_dict_after_key = |_, state| Ok(state)

	parse_dict_after_entry : Format, State -> Try([Continue(State), Done(State)], [FormatError])
	parse_dict_after_entry = |_, state| Ok(Done(state))

	parse_key_str : Format, State -> Try({ value : Str, rest : State }, [FormatError])
	parse_key_str = |_, _| Err(FormatError)
}

State := [Present(Str)]

parse : Str -> Try(a, [FormatError])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

main : Try(Dict(Str, Str), [FormatError])
main = parse("foo: bar")
