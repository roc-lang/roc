ParserTopLevelStoredInputWrapper :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name| name

	parse_str : State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(MissingRequired)
		}

	parse_record_field : Fields(_shape), State -> Try(
		[
			Field({ field : Field(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : State -> Try(State, [MissingRequired])
	skip_record_field = |_| Ok(Done)

	missing_record_field : Str, State -> [MissingRequired]
	missing_record_field = |_, _| MissingRequired
}

State := [Present(Str), Done]

parser_for : () -> (Str -> Try(a, [MissingRequired]))
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parser_for = || {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)

	|input| {
		parsed = parse_shape(State.Present(input))?
		Ok(parsed.value)
	}
}

parse_stored : Str -> Try({ foo : Str }, [MissingRequired])
parse_stored = parser_for()

expect {
	result = parse_stored("stored")?

	result == { foo: "stored" }
}
