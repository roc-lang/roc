ParserOldTagSpecApi :: [].{}

Format := [Default].{
	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [FormatError])
	parse_tag_union = |_, spec, state|
		match state {
			Present(tag_name) => {
				value = Encoding.ParseTagUnionSpec.parse(spec, tag_name, State.Present(tag_name), Str.is_eq, FormatError)?
				Ok({ value, rest: State.Present("") })
			}
		}
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

main : Try([One], [FormatError])
main = parse("One")
