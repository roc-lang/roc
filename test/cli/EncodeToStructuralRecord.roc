EncodeToStructuralRecord :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name|
		if Str.is_eq(name, "foo_bar") {
			"foo-bar"
		} else {
			name
		}

	begin_record : U64 -> Try(U64, [])
	begin_record = |state| Ok(state + 1)

	encode_record_field : Str, U64 -> Try(U64, [])
	encode_record_field = |name, state| Ok(state + Str.count_utf8_bytes(name))

	end_record : U64 -> Try(U64, [])
	end_record = |state| Ok(state + 2)

	encode_str : Str, U64 -> Try(U64, [])
	encode_str = |value, state| Ok(state + Str.count_utf8_bytes(value))

	encode_u64 : U64, U64 -> Try(U64, [])
	encode_u64 = |value, state| Ok(state + value)
}

Token := { raw : Str }.{
	encode_to : Token, Format -> (U64 -> Try(U64, []))
	encode_to = |token, _| |state| Format.encode_str(token.raw, state)
}

encode : value -> Try(U64, [])
	where [
		value.encode_to : value, Format -> (U64 -> Try(U64, [])),
	]
encode = |value| {
	encode_value = value.encode_to(Format.Default)
	encode_value(0)
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
