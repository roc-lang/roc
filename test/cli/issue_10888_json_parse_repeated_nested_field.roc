Issue10888JsonParseRepeatedNestedField :: [].{}

RepeatedNestedField : { w : { w : { f1 : Str } } }

expect {
	parsed : Try(RepeatedNestedField, [InvalidJson(Str), MissingRequiredField(Str)])
	parsed = Json.parse("{\"w\":{\"w\":{\"f1\":\"ok\"}}}")

	match parsed {
		Ok(value) => value.w.w.f1 == "ok"
		Err(_) => False
	}
}

parse_stored : Str -> Try(RepeatedNestedField, [InvalidJson(Str), MissingRequiredField(Str)])
parse_stored = Json.parser_camel()

expect {
	parsed = parse_stored("{\"w\":{\"w\":{\"f1\":\"stored\"}}}")?

	parsed.w.w.f1 == "stored"
}
