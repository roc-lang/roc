# An error row that no annotation bounds gains MissingRequiredField(Str) from
# the derived parser itself, even when nothing in the program names the tag.
ParserMissingFieldOpenRow :: [].{}

expect {
	v = Json.parse("{}")
	missing_x = match v {
		Ok({ x }) => x == "unused"
		Err(_) => False
	}
	!missing_x and Str.inspect(v) == "Err(MissingRequiredField(\"x\"))"
}

expect {
	v : Try({ x : Str, y : U8 }, _)
	v = Json.parse("{\"x\":\"a\"}")
	Str.inspect(v) == "Err(MissingRequiredField(\"y\"))"
}
