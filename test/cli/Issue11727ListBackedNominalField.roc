# repro for https://github.com/roc-lang/roc/issues/11727
# A derived container (record, list, or tuple) whose element is a custom-coded
# nominal type backed directly by `List(U8)` must compile and parse correctly.
# The type works when parsed on its own; the compiler must also finish building
# the derived record parser below and parse `{"b": "hi"}` successfully.
app [main!] { pf: platform "../fx/platform/main.roc" }

Blob := List(U8).{
	parser_for = |format| |state| {
		parsed = Json.parse_str(format, state)?
		Ok({ value: Blob.(parsed.value.to_utf8()), rest: parsed.rest })
	}
}

expect {
	blob_alone : Try(Blob, _)
	blob_alone = Json.parse("\"hi\"")
	blob_alone.is_ok()
}

expect {
	in_record : Try({ b : Blob }, _)
	in_record = Json.parse("{\"b\": \"hi\"}")
	in_record.is_ok()
}

main! = || {}
