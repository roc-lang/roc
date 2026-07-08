ParserTopLevelStoredValidatedWrapper :: [].{
	DecodeErr := [MissingRequired, TrailingInput].{}
}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, ParserTopLevelStoredValidatedWrapper.DecodeErr)
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(ParserTopLevelStoredValidatedWrapper.DecodeErr.MissingRequired)
		}

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		ParserTopLevelStoredValidatedWrapper.DecodeErr,
	)
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, ParserTopLevelStoredValidatedWrapper.DecodeErr)
	skip_record_field = |_, _| Ok(Done)

	missing_record_field : Format, Str, State -> ParserTopLevelStoredValidatedWrapper.DecodeErr
	missing_record_field = |_, _, _| ParserTopLevelStoredValidatedWrapper.DecodeErr.MissingRequired
}

State := [Present(Str), Done]

trailing_input : ParserTopLevelStoredValidatedWrapper.DecodeErr
trailing_input = ParserTopLevelStoredValidatedWrapper.DecodeErr.TrailingInput

parser_for : () -> (Str -> Try(a, ParserTopLevelStoredValidatedWrapper.DecodeErr))
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, ParserTopLevelStoredValidatedWrapper.DecodeErr)),
	]
parser_for = || {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)

	|input| {
		parsed = parse_shape(State.Present(input))?

		match parsed.rest {
			Done => Ok(parsed.value)
			Present(_) => Err(trailing_input)
		}
	}
}

parse_stored : Str -> Try({ foo : Str }, ParserTopLevelStoredValidatedWrapper.DecodeErr)
parse_stored = parser_for()

expect {
	result = parse_stored("stored")?

	result == { foo: "stored" }
}
