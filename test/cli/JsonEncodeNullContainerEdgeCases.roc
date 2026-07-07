JsonEncodeNullContainerEdgeCases :: [].{}

expect {
	result : Try(Str, Json.ParseErr)
	result = Json.parse("null")
	result == Err(Json.invalid_json)
}

expect {
	result : Try(List(Str), Json.ParseErr)
	result = Json.parse("[\"a\",null]")
	result == Err(Json.invalid_json)
}

expect {
	result : Try(List(Try(Str, [Null])), Json.ParseErr)
	result = Json.parse("[\"a\",null]")
	result == Ok([Ok("a"), Err(Null)])
}

expect {
	result : Try({ optional : Try(Str, [Missing]) }, Json.ParseErr)
	result = Json.parse("{}")
	result == Ok({ optional: Err(Missing) })
}

expect {
	result : Try({ optional : Try(Str, [Missing]) }, Json.ParseErr)
	result = Json.parse("{\"optional\":null}")
	result == Err(Json.invalid_json)
}

expect {
	value : { optional : Try(Str, [Missing]) }
	value = { optional: Err(Missing) }

	Json.to_str(value) == "{}"
}

expect {
	missing_result : Try({ field : Try(Str, [Missing, Null]) }, Json.ParseErr)
	missing_result = Json.parse("{}")

	null_result : Try({ field : Try(Str, [Missing, Null]) }, Json.ParseErr)
	null_result = Json.parse("{\"field\":null}")

	value_result : Try({ field : Try(Str, [Missing, Null]) }, Json.ParseErr)
	value_result = Json.parse("{\"field\":\"value\"}")

	missing_result == Ok({ field: Err(Missing) })
		and null_result == Ok({ field: Err(Null) })
			and value_result == Ok({ field: Ok("value") })
}

expect {
	value : { field : Try(Str, [Null]) }
	value = { field: Err(Null) }

	Json.to_str(value) == "{\"field\":null}"
}

expect {
	result : Try({ field : Try(Str, [Null]) }, Json.ParseErr)
	result = Json.parse("{}")
	result == Err(MissingRequiredField("field"))
}

expect {
	result : Try({}, Json.ParseErr)
	result = Json.parse("{}")
	result == Ok({})
}

expect {
	value = {}

	Json.to_str(value) == "{}"
}
