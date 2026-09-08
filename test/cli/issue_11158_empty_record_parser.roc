Issue11158EmptyRecordParser :: [].{}

Format := [Default].{
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
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done(state))
		}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError, ..])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

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

# No rename_field method is needed when there are no requested field names.
expect parse(State.Done) == Ok({})
expect parse(State.Present("ignored")) == Ok({})

expect {
	parsed : Try({}, [FormatError])
	parsed = parse(State.Done)
	parsed == Ok({})
}

# Inference from the literal must agree with an explicit empty-record type.
expect Json.parse("{}") == Ok({})

parse_empty : Str -> Try({}, [InvalidJson(Str)])
parse_empty = |input| Json.parse(input)

expect parse_empty("{}") == Ok({})
expect Json.parse("{\"unknown\": [1, {\"nested\": true}]}") == Ok({})

expect match Json.parse("{") {
	Err(InvalidJson(_)) => True
	Ok({}) => False
}

expect match Json.parse("{\"unknown\": }") {
	Err(InvalidJson(_)) => True
	Ok({}) => False
}

expect match Json.parse("[]") {
	Err(InvalidJson(_)) => True
	Ok({}) => False
}

# A derivation containing only empty records has no rename call to share.
expect Json.parse("[{}, {}]") == Ok([{}, {}])
expect Json.parse("{\"inner\": {}}") == Ok({ inner: {} })
