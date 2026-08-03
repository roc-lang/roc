EncoderForMissingCustomNominalMethod :: [].{}

Token := { raw : Str }.{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
	encode_record = |state, _, write_fields|
		write_fields(state, |field_state, _, write_value| write_value(field_state))

	encode_str : Str, U64 -> Try(U64, [])
	encode_str = |value, state| Ok(state + Str.count_utf8_bytes(value))
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

main : Try(U64, [])
main = encode({ token: Token.{ raw: "abc" } })
