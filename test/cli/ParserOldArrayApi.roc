ParserOldArrayApi :: [].{}

## The array-shaped protocol a format used before lists and tuples were
## distinguished. It is no longer part of the protocol, so a shape that needs
## a list must be rejected rather than silently parsed as an array.
Format := [Default].{
	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError, ..])
	parse_str = |_| Err(FormatError)

	parse_array_start : Format, State -> Try(State, [FormatError, ..])
	parse_array_start = |_, state| Ok(state)

	parse_array_next : Format, State -> Try([Element(State), Done(State)], [FormatError, ..])
	parse_array_next = |_, state| Ok(Done(state))

	parse_array_after_element : Format, State -> Try([Continue(State), Done(State)], [FormatError, ..])
	parse_array_after_element = |_, state| Ok(Done(state))
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

main : Try(List(Str), [FormatError])
main = parse("anything")
