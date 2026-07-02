JsonEncodeNullContainerEdgeCases :: [].{}

expect {
	result : Try(Str, Json)
	result = Json.parse("null")
	result == Err(Json.invalid_json)
}

expect {
	result : Try(List(Str), Json)
	result = Json.parse("[\"a\",null]")
	result == Err(Json.invalid_json)
}

expect {
	result : Try(List(Try(Str, [Null])), Json)
	result = Json.parse("[\"a\",null]")
	result == Ok([Ok("a"), Err(Null)])
}

expect {
	result : Try({ optional : Try(Str, [Missing]) }, Json)
	result = Json.parse("{}")
	result == Ok({ optional: Err(Missing) })
}

expect {
	result : Try({ optional : Try(Str, [Missing]) }, Json)
	result = Json.parse("{\"optional\":null}")
	result == Err(Json.invalid_json)
}

expect {
	value : { optional : Try(Str, [Missing]) }
	value = { optional: Err(Missing) }

	encoded_result : Try(Str, [])
	encoded_result = Json.encode(value)
	encoded_result == Ok("{}")
}

expect {
	missing_result : Try({ field : Try(Str, [Missing, Null]) }, Json)
	missing_result = Json.parse("{}")

	null_result : Try({ field : Try(Str, [Missing, Null]) }, Json)
	null_result = Json.parse("{\"field\":null}")

	value_result : Try({ field : Try(Str, [Missing, Null]) }, Json)
	value_result = Json.parse("{\"field\":\"value\"}")

	missing_result == Ok({ field: Err(Missing) })
		and null_result == Ok({ field: Err(Null) })
			and value_result == Ok({ field: Ok("value") })
}

expect {
	value : { field : Try(Str, [Null]) }
	value = { field: Err(Null) }

	encoded_result : Try(Str, [])
	encoded_result = Json.encode(value)
	encoded_result == Ok("{\"field\":null}")
}

expect {
	result : Try({ field : Try(Str, [Null]) }, Json)
	result = Json.parse("{}")
	result == Err(Json.MissingRequired)
}

expect {
	result : Try({}, Json)
	result = Json.parse("{}")
	result == Ok({})
}

expect {
	value = {}

	encoded_result : Try(Str, [])
	encoded_result = Json.encode(value)
	encoded_result == Ok("{}")
}
