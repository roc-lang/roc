# Even without an annotation, missing fields use only the format's error.
ParserMissingFieldOpenRow :: [].{}

expect {
	v = Json.parse("{}")
	missing_x = match v {
		Ok({ x }) => x == "unused"
		Err(_) => False
	}
	!missing_x and Str.inspect(v) == "Err(InvalidJson(\"Missing required field: x\"))"
}

expect {
	v : Try({ x : Str, y : U8 }, _)
	v = Json.parse("{\"x\":\"a\"}")
	Str.inspect(v) == "Err(InvalidJson(\"Missing required field: y\"))"
}
