EncodeToTopLevelStored :: [].{}

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

value : { count : U64, foo_bar : Str }
value = { count: 7, foo_bar: "abc" }

encode_stored : U64 -> Try(U64, [])
encode_stored = value.encode_to(Format.Default)

expect encode_stored(0) == Ok(25)
