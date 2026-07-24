ParserTopLevelStoredParserConstructor :: [].{}

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

make_parser : () -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs]))
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
	]
make_parser = || {
	Shape : a
	Shape.parser_for(Format.Default)
}

parse_stored : State -> Try({ value : { foo : Str }, rest : State }, [FormatError, MissingRequiredField(Str)])
parse_stored = make_parser()

expect {
	result = parse_stored(State.Present("stored"))?

	result.value == { foo: "stored" }
}
