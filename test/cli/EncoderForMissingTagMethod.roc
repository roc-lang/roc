EncoderForMissingTagMethod :: [].{}

Format := [Default].{
	encode_str : Str, U64 -> Try(U64, [])
	encode_str = |_, state| Ok(state)
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
main = {
	value : [One]
	value = One

	encode(value)
}
