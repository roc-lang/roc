JsonParseErrorComposition :: [].{}

# A scalar JSON parser has no derived-record missing-field failure.
expect {
	result : Try(F64, [InvalidJson(Str)])
	result = Json.parse("oops")

	result.map_err(|InvalidJson(message)| MyError(message)) == Err(MyError("Invalid JSON"))
}

# Required fields use JSON's error type and identify the absent field.
expect {
	result : Try({ length : F64 }, [InvalidJson(Str)])
	result = Json.parse("{}")

	result == Err(InvalidJson("Missing required field: length"))
}

# A record containing only optional parser fields cannot produce that error.
expect {
	result : Try({ length : Try(F64, [Missing]) }, [InvalidJson(Str)])
	result = Json.parse("{}")

	result == Ok({ length: Err(Missing) })
}

Token := { raw : Str }.{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: { raw: parsed.value }, rest: parsed.rest })
	}
}

expect {
	result : Try({ token : Token }, [InvalidJson(Str)])
	result = Json.parse("{\"token\":\"ok\"}")

	result.is_ok()
}

## Inference keeps the same format error for scalar and structural parsing.
parse_length : Str -> Try({ length : F64 }, _)
parse_length = |json| Json.parse(json)

expect parse_length("{}") == Err(InvalidJson("Missing required field: length"))

expect parse_length("{\"length\":2.5}") == Ok({ length: 2.5 })
