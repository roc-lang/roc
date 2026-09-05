EncoderForOptionalRecordField :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

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

encode : value -> Try(List(Str), [])
	where [
		value.encoder_for : Format -> (value, List(Str) -> Try(List(Str), [])),
	]
encode = |value| {
	Shape : value
	encode_value = Shape.encoder_for(Format.Default)
	encode_value(value, [])
}

encode_attrs : { count : U64, label ?: Str } -> Try(List(Str), [])
encode_attrs = |attrs| encode(attrs)

expect encode_attrs({ count: 3, label: "x" }) == Ok(["record", "count", "3", "label", "x", "end"])

expect encode_attrs({ count: 5 }) == Ok(["record", "count", "5", "end"])
