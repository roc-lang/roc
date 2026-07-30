ParserUnsupportedNumericField :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError, ..])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		[FormatError, ..],
	)
	parse_record_field = |_, _, state| Ok(Done(state))

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError, ..])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, state| Ok(state)
}

State := [Done]

parse : State -> Try(a, [FormatError, ..errs])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

main : Try({ count : I64 }, [FormatError, MissingRequiredField(Str)])
main = parse(State.Done)
