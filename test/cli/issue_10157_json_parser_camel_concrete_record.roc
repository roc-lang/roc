# repro for https://github.com/roc-lang/roc/issues/10157
# Binding Json.parser_camel() at a concrete record type must compile.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |_args| {
	parse : Str -> Try({ privacy_status : Str }, _)
	parse = Json.parser_camel()
	_ = parse

	Ok({})
}
