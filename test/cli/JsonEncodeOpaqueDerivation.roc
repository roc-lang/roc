JsonEncodeOpaqueDerivation :: [].{}

OpaqueAutoToken :: { raw : Str, count : U64 }.{
	parser_for : _
	encoder_for : _
}

opaque_auto_token_encodes : Str -> Bool
opaque_auto_token_encodes = |json| {
	parsed : Try(OpaqueAutoToken, [InvalidJson(Str), MissingRequiredField(Str)])
	parsed = Json.parse(json)

	match parsed {
		Ok(value) => {
			Json.to_str(value) == "{\"count\":7,\"raw\":\"opaque\"}"
		}
		Err(_) => False
	}
}

expect opaque_auto_token_encodes("{\"raw\":\"opaque\",\"count\":7}")
