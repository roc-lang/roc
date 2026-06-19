ParserTopLevelStoredValidatedWrapper :: [].{
	DecodeErr := [MissingRequired, TrailingInput].{}
}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name| name

	parse_str : State -> Try({ value : Str, rest : State }, ParserTopLevelStoredValidatedWrapper.DecodeErr)
	parse_str = |state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(ParserTopLevelStoredValidatedWrapper.DecodeErr.MissingRequired)
		}

	parse_record_field : Fields(_shape), State -> Try(
		[
			Field({ field : Field(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		ParserTopLevelStoredValidatedWrapper.DecodeErr,
	)
	parse_record_field = |_, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : State -> Try(State, ParserTopLevelStoredValidatedWrapper.DecodeErr)
	skip_record_field = |_| Ok(Done)

	missing_record_field : Str, State -> ParserTopLevelStoredValidatedWrapper.DecodeErr
	missing_record_field = |_, _| ParserTopLevelStoredValidatedWrapper.DecodeErr.MissingRequired
}

State := [Present(Str), Done]

trailing_input : ParserTopLevelStoredValidatedWrapper.DecodeErr
trailing_input = ParserTopLevelStoredValidatedWrapper.DecodeErr.TrailingInput

parser : () -> (Str -> Try(a, ParserTopLevelStoredValidatedWrapper.DecodeErr))
	where [
		a.parser : Format -> (State -> Try({ value : a, rest : State }, ParserTopLevelStoredValidatedWrapper.DecodeErr)),
	]
parser = || {
	Shape : a
	parse_shape = Shape.parser(Format.Default)

	|input| {
		parsed = parse_shape(State.Present(input))?

		match parsed.rest {
			Done => Ok(parsed.value)
			Present(_) => Err(trailing_input)
		}
	}
}

parse_stored : Str -> Try({ foo : Str }, ParserTopLevelStoredValidatedWrapper.DecodeErr)
parse_stored = parser()

expect {
	result = parse_stored("stored")?

	result == { foo: "stored" }
}
