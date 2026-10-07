# A derived record parser with a required field can fail with
# MissingRequiredField(Str), so an annotation whose error row omits that tag
# is a type mismatch, whether it annotates a value or a function.
Issue11561ClosedErrorRow :: [].{}

parse_x : Str -> Try({ x : Str }, [InvalidJson(Str)])
parse_x = |s| Json.parse(s)

expect {
	v : Try({ x : Str }, [InvalidJson(Str)])
	v = Json.parse("{}")
	v.is_err()
}

expect parse_x("{}").is_err()
