ParserRequiredFieldError :: [].{}

# This format explicitly owns the missing-field tag; every operation uses it.
ParseError : [FormatError, MissingRequiredField(Str)]

Format := [Default].{
	missing_field : Format, Str -> ParseError
	missing_field = |_, name| MissingRequiredField(name)

	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, ParseError)
	parse_str = |_, _state| Err(FormatError)

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], ParseError)
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
		ParseError,
	)
	parse_record_field = |_, _, state| Ok(Done(state))

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], ParseError)
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, ParseError)
	skip_record_field = |_, state| Ok(state)

	# This deliberately has the wrong result type for the parser protocol. A
	# derived record parser calls missing_field, never invalid_value, so this
	# declaration must not be constrained.
	invalid_value : Format, State -> {}
	invalid_value = |_, _| {}
}

State := [Done]

parse : State -> Try(a, ParseError)
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, ParseError)),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

expect {
	result : Try({ name : Str }, ParseError)
	result = parse(Done)

	result == Err(MissingRequiredField("name"))
}
