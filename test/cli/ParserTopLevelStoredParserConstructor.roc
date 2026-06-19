ParserTopLevelStoredParserConstructor :: [].{}

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

make_parser : () -> (State -> Try({ value : a, rest : State }, [MissingRequired]))
	where [
		a.parser : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
make_parser = || {
	Shape : a
	Shape.parser(Format.Default)
}

parse_stored : State -> Try({ value : { foo : Str }, rest : State }, [MissingRequired])
parse_stored = make_parser()

expect {
	result = parse_stored(State.Present("stored"))?

	result.value == { foo: "stored" }
}
