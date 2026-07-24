ParserTopLevelStoredInputWrapper :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError, ..])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(FormatError)
		}

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[FormatError, ..],
	)
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

parser_for : () -> (Str -> Try(a, [FormatError, ..errs]))
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
	]
parser_for = || {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)

	|input| {
		parsed = parse_shape(State.Present(input))?
		Ok(parsed.value)
	}
}

parse_stored : Str -> Try({ foo : Str }, [FormatError, MissingRequiredField(Str)])
parse_stored = parser_for()

expect {
	result = parse_stored("stored")?

	result == { foo: "stored" }
}
