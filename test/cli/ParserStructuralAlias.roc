ParserStructuralAlias :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

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

main : () -> Try({ foo : Str }, [MissingRequired])
main = || {
	Shape : { foo : Str }

	parse_shape : State -> Try({ value : { foo : Str }, rest : State }, [MissingRequired])
	parse_shape = Shape.parser_for(Format.Default)

	parsed = parse_shape(State.Present(""))?
	Ok(parsed.value)
}
