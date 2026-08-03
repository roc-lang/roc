EncoderForTags :: [].{}

Format := [Default].{
	encode_tag : List(Str), Str, U64, (List(Str), (List(Str), (List(Str) -> Try(List(Str), [])) -> Try(List(Str), [])) -> Try(List(Str), [])) -> Try(List(Str), [])
	encode_tag = |state, name, count, write_payloads| {
		count_text = if count == 2 "two" else "other"
		started = List.append(List.append(List.append(state, "tag"), name), count_text)
		write_payloads(
			started,
			|payload_state, write_value| write_value(List.append(payload_state, "payload")),
		)
	}

	encode_str : Str, List(Str) -> Try(List(Str), [])
	encode_str = |_, state| Ok(List.append(state, "str"))

	encode_u64 : U64, List(Str) -> Try(List(Str), [])
	encode_u64 = |_, state| Ok(List.append(state, "u64"))
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

expect {
	value : [Pair(Str, U64)]
	value = Pair("x", 9)

	encode(value) == Ok(["tag", "Pair", "two", "payload", "str", "payload", "u64"])
}

expect encode("Pair") == Ok(["str"])
