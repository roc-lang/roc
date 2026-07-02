JsonEncodeOpaqueDerivation :: [].{}

OpaqueAutoToken :: { raw : Str, count : U64 }.{
	parser_for : _
	encode_to : _
}

opaque_auto_token_encodes : Str -> Bool
opaque_auto_token_encodes = |json| {
	parsed : Try(OpaqueAutoToken, Json)
	parsed = Json.parse(json)

	match parsed {
		Ok(value) => {
			encoded_result : Try(Str, [])
			encoded_result = Json.encode(value)
			encoded_result == Ok("{\"count\":7,\"raw\":\"opaque\"}")
		}
		Err(_) => False
	}
}

expect opaque_auto_token_encodes("{\"raw\":\"opaque\",\"count\":7}")
