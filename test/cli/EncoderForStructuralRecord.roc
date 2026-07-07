EncoderForStructuralRecord :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name|
		if Str.is_eq(name, "foo_bar") {
			"foo-bar"
		} else {
			name
		}

	encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
	encode_record = |state, _, write_fields| {
		started = state + 1
		finished = write_fields(
			started,
			|field_state, name, write_value| {
				named = field_state + Str.count_utf8_bytes(name)
				write_value(named)
			},
		)?
		Ok(finished + 2)
	}

	encode_str : Str, U64 -> Try(U64, [])
	encode_str = |value, state| Ok(state + Str.count_utf8_bytes(value))

	encode_u64 : U64, U64 -> Try(U64, [])
	encode_u64 = |value, state| Ok(state + value)
}

Token := { raw : Str }.{
	encoder_for : Format -> (Token, U64 -> Try(U64, []))
	encoder_for = |_| |token, state| Format.encode_str(token.raw, state)
}

encode : value -> Try(U64, [])
	where [
		value.encoder_for : Format -> (value, U64 -> Try(U64, [])),
	]
encode = |value| {
	Shape : value
	encode_value = Shape.encoder_for(Format.Default)
	encode_value(value, 0)
}

expect {
	value : {
		count : U64,
		foo_bar : Str,
		nested : { token : Token },
	}
	value = {
		count: 7,
		foo_bar: "abc",
		nested: {
			token: Token.{ raw: "zz" },
		},
	}

	encode(value) == Ok(41)
}
