EncoderForTopLevelStoredOptionalField :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name|
		if Str.is_eq(name, "foo_bar") {
			"foo-bar"
		} else {
			name
		}

	encode_record : List(Str), U64, (List(Str), (List(Str), Str, (List(Str) -> Try(List(Str), [])) -> Try(List(Str), [])) -> Try(List(Str), [])) -> Try(List(Str), [])
	encode_record = |state, _, write_fields| {
		started = List.append(state, "record")
		finished = write_fields(
			started,
			|field_state, name, write_value| write_value(List.append(field_state, name)),
		)?
		Ok(List.append(finished, "end"))
	}

	encode_str : Str, List(Str) -> Try(List(Str), [])
	encode_str = |value, state| Ok(List.append(state, value))

	encode_u64 : U64, List(Str) -> Try(List(Str), [])
	encode_u64 = |value, state| Ok(List.append(state, value.to_str()))
}

Value : { count : U64, foo_bar : Str, note ?: Str }

value : Value
value = { count: 7, foo_bar: "abc" }

encoder_for_value : value -> (value, List(Str) -> Try(List(Str), []))
	where [
		value.encoder_for : Format -> (value, List(Str) -> Try(List(Str), [])),
	]
encoder_for_value = |_| {
	Shape : value
	Shape.encoder_for(Format.Default)
}

encode_stored : Value, List(Str) -> Try(List(Str), [])
encode_stored = encoder_for_value(value)

# A `List(Str)` accumulator rather than the summed `U64` this fixture used
# first: addition is order-insensitive, so the old `== Ok(25)` held under any
# field order and under a `rename_field` that did nothing. This pins both the
# order the generated encoder visits fields in and the rename of `foo_bar`,
# and it leaves the unset `note ?:` field visibly absent.
expect encode_stored(value, []) == Ok(["record", "count", "7", "foo-bar", "abc", "end"])
