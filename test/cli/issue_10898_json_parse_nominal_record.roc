app [program] { pf: platform "./issue_10898_json_parse_nominal_record_platform.roc" }

FooDto := {
	foo : Str,
}.{
	parser_for : _
}

program = |_| {
	parsed : Try(FooDto, [InvalidJson(Str), MissingRequiredField(Str)])
	parsed = Json.parse("{\"foo\":\"ok\"}")
	_ = parsed
	{}
}
