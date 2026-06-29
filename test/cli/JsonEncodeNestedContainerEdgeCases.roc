JsonEncodeNestedContainerEdgeCases :: [].{}

list_record_parses_and_encodes : Str -> Bool
list_record_parses_and_encodes = |json| {
	result : Try(List({ foo : Str }), Json)
	result = Json.parse(json)

	match result {
		Ok(parsed) => {
			encoded_result : Try(Str, [])
			encoded_result = Json.encode(parsed)
			encoded_result == Ok("[{\"foo\":\"a\"},{\"foo\":\"b\"}]")
		}
		Err(_) => False
	}
}

expect list_record_parses_and_encodes("[{\"foo\":\"a\"},{\"foo\":\"b\"}]")

list_tuple_record_parses_and_encodes : Str -> Bool
list_tuple_record_parses_and_encodes = |json| {
	result : Try(List({ points : (Str, U64) }), Json)
	result = Json.parse(json)

	match result {
		Ok(parsed) => {
			encoded_result : Try(Str, [])
			encoded_result = Json.encode(parsed)
			encoded_result == Ok("[{\"points\":[\"left\",1]},{\"points\":[\"right\",2]}]")
		}
		Err(_) => False
	}
}

expect list_tuple_record_parses_and_encodes("[{\"points\":[\"left\",1]},{\"points\":[\"right\",2]}]")

list_tag_record_parses_and_encodes : Str -> Bool
list_tag_record_parses_and_encodes = |json| {
	result : Try(List({ status : [Active, Paused] }), Json)
	result = Json.parse(json)

	match result {
		Ok(parsed) => {
			encoded_result : Try(Str, [])
			encoded_result = Json.encode(parsed)
			encoded_result == Ok("[{\"status\":\"Active\"},{\"status\":\"Paused\"}]")
		}
		Err(_) => False
	}
}

expect list_tag_record_parses_and_encodes("[{\"status\":\"Active\"},{\"status\":\"Paused\"}]")

list_two_field_record_parses_and_encodes : Str -> Bool
list_two_field_record_parses_and_encodes = |json| {
	result : Try(List({ foo : Str, bar : U64 }), Json)
	result = Json.parse(json)

	match result {
		Ok(parsed) => {
			encoded_result : Try(Str, [])
			encoded_result = Json.encode(parsed)
			encoded_result == Ok("[{\"bar\":1,\"foo\":\"a\"},{\"bar\":2,\"foo\":\"b\"}]")
		}
		Err(_) => False
	}
}

expect list_two_field_record_parses_and_encodes("[{\"foo\":\"a\",\"bar\":1},{\"foo\":\"b\",\"bar\":2}]")

list_tag_tuple_record_parses_and_encodes : Str -> Bool
list_tag_tuple_record_parses_and_encodes = |json| {
	result : Try(List({ status : [Active, Paused], points : (Str, U64) }), Json)
	result = Json.parse(json)

	match result {
		Ok(parsed) => {
			encoded_result : Try(Str, [])
			encoded_result = Json.encode(parsed)
			encoded_result == Ok("[{\"points\":[\"left\",1],\"status\":\"Active\"},{\"points\":[\"right\",2],\"status\":\"Paused\"}]")
		}
		Err(_) => False
	}
}

expect list_tag_tuple_record_parses_and_encodes("[{\"status\":\"Active\",\"points\":[\"left\",1]},{\"status\":\"Paused\",\"points\":[\"right\",2]}]")
