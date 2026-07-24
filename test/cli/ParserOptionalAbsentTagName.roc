ParserOptionalAbsentTagName :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError, ..])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(FormatError)
		}

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

expect {
	result : Try({ foo : Try(Str, [Missing]) }, [FormatError])
	result = parse(State.Done)

	result == Ok({ foo: Err(Missing) })
}
