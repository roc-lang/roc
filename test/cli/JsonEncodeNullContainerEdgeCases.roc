JsonEncodeNullContainerEdgeCases :: [].{}

expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("null")
	result == Err(Json.invalid_json)
}

expect {
	result : Try(List(Str), [InvalidJson(Str)])
	result = Json.parse("[\"a\",null]")
	result == Err(Json.invalid_json)
}

expect {
	result : Try(List(Try(Str, [Null])), [InvalidJson(Str)])
	result = Json.parse("[\"a\",null]")
	result == Ok([Ok("a"), Err(Null)])
}

expect {
	result : Try({ optional : Try(Str, [Missing]) }, [InvalidJson(Str)])
	result = Json.parse("{}")
	result == Ok({ optional: Err(Missing) })
}

expect {
	result : Try({ optional : Try(Str, [Missing]) }, [InvalidJson(Str)])
	result = Json.parse("{\"optional\":null}")
	result == Err(Json.invalid_json)
}

expect {
	value : { optional : Try(Str, [Missing]) }
	value = { optional: Err(Missing) }

	Json.to_str(value) == "{}"
}

expect {
	missing_result : Try({ field : Try(Try(Str, [Null]), [Missing]) }, [InvalidJson(Str)])
	missing_result = Json.parse("{}")

	null_result : Try({ field : Try(Try(Str, [Null]), [Missing]) }, [InvalidJson(Str)])
	null_result = Json.parse("{\"field\":null}")

	value_result : Try({ field : Try(Try(Str, [Null]), [Missing]) }, [InvalidJson(Str)])
	value_result = Json.parse("{\"field\":\"value\"}")

	missing_result == Ok({ field: Err(Missing) })
		and null_result == Ok({ field: Ok(Err(Null)) })
			and value_result == Ok({ field: Ok(Ok("value")) })
}

expect {
	value : { field : Try(Str, [Null]) }
	value = { field: Err(Null) }

	Json.to_str(value) == "{\"field\":null}"
}

expect {
	result : Try({ field : Try(Str, [Null]) }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{}")
	result == Err(MissingRequiredField("field"))
}

expect {
	result : Try({}, [InvalidJson(Str)])
	result = Json.parse("{}")
	result == Ok({})
}

expect {
	value = {}

	Json.to_str(value) == "{}"
}
