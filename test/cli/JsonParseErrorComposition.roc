JsonParseErrorComposition :: [].{}

# A scalar JSON parser has no derived-record missing-field failure.
expect {
	result : Try(F64, [InvalidJson(Str)])
	result = Json.parse("oops")

	result.map_err(|InvalidJson(message)| MyError(message)) == Err(MyError("Invalid JSON"))
}

# A required record field composes the generic parser error into JSON's error row.
expect {
	result : Try({ length : F64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{}")

	result == Err(MissingRequiredField("length"))
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
	result : Try({ token : Token }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"token\":\"ok\"}")

	result.is_ok()
}
