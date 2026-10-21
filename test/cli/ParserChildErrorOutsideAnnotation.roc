# A custom nominal parser nested in a derived record contributes its own error
# tags to the definition that parses the record, so an annotation that omits
# one of them is an error at the parser. An annotated value's row is the
# annotation's at every use, so a match that names another tag is a mismatch
# of its own.
ParserChildErrorOutsideAnnotation :: [].{}

Token := { raw : Str }.{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		if parsed.value == "bad" Err(Oops) else Ok({ value: { raw: parsed.value }, rest: parsed.rest })
	}
}

top_value : Try({ token : Token }, [InvalidJson(Str), MissingRequiredField(Str)])
top_value = Json.parse("{\"token\":\"bad\"}")

top_fn : Str -> Try({ token : Token }, [InvalidJson(Str), MissingRequiredField(Str)])
top_fn = |s| Json.parse(s)

widened : Try({ token : Token }, [InvalidJson(Str), MissingRequiredField(Str)])
widened = Json.parse("{\"token\":\"bad\"}")

expect {
	local : Try({ token : Token }, [InvalidJson(Str), MissingRequiredField(Str)])
	local = Json.parse("{\"token\":\"bad\"}")
	local.is_err() and top_value.is_err() and top_fn("").is_err()
}

expect {
	match widened {
		Err(Unrelated) => False
		_ => True
	}
}
