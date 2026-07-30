EncoderForEmptyRecordNoFieldMethods :: [].{}

Format := [Default].{
	encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
	encode_record = |state, _, write_fields| {
		started = state + 1
		finished = write_fields(started, |field_state, _, write_value| write_value(field_state))?
		Ok(finished + 2)
	}
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

expect encode({}) == Ok(3)
