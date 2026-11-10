ParserOldRecordFieldApi :: [].{}

Format := [Default].{
	missing_field : Format, Str -> [FormatError]
	missing_field = |_, _| FormatError

	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError])
	parse_str = |_, _| Err(FormatError)

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [FormatError])
	skip_record_field = |_, state| Ok(state)

	parse_record_field : State -> Try(
		[
			Field({ name : Str, value : State, rest : State }),
			End({ rest : State, missing : [FormatError] }),
		],
		[FormatError],
	)
	parse_record_field = |_| Err(FormatError)
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

main : Try({ foo : Str }, [FormatError])
main = parse("foo")
