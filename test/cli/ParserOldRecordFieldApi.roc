ParserOldRecordFieldApi :: [].{}

Format := [Default].{
	parse_str : State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_| Err(MissingRequired)

	parse_record_field : State -> Try(
		[
			Field({ name : Str, value : State, rest : State }),
			End({ rest : State, missing : [MissingRequired] }),
		],
		[MissingRequired],
	)
	parse_record_field = |_| Err(MissingRequired)
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

main : Try({ foo : Str }, [MissingRequired])
main = parse("foo")
