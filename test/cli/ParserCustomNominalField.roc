ParserCustomNominalField :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(MissingRequired)
		}

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "token", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, _| Ok(Done)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired
}

State := [Present(Str), Done]

Token := { raw : Str }.{
	parser_for : Format -> (State -> Try({ value : Token, rest : State }, [MissingRequired]))
	parser_for = |format| |state| {
		parsed = Format.parse_str(format, state)?
		Ok({ value: { raw: "custom-token" }, rest: parsed.rest })
	}

	count_utf8_bytes : Token -> U64
	count_utf8_bytes = |token| Str.count_utf8_bytes(token.raw)
}

parse : State -> Try(a, [MissingRequired])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

expect {
	result : Try({ token : Token }, [MissingRequired])
	result = parse(State.Present("wire-token"))

	match result {
		Ok(decoded) => Token.count_utf8_bytes(decoded.token) == 12
		Err(_) => False
	}
}
