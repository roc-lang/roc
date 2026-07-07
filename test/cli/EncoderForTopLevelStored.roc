EncoderForTopLevelStored :: [].{}

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

Value : { count : U64, foo_bar : Str }

value : Value
value = { count: 7, foo_bar: "abc" }

encoder_for_value : value -> (value, U64 -> Try(U64, []))
	where [
		value.encoder_for : Format -> (value, U64 -> Try(U64, [])),
	]
encoder_for_value = |_| {
	Shape : value
	Shape.encoder_for(Format.Default)
}

encode_stored : Value, U64 -> Try(U64, [])
encode_stored = encoder_for_value(value)

expect encode_stored(value, 0) == Ok(25)
