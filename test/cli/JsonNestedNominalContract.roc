JsonNestedNominalContract :: [].{}

Req := { name : Str }.{
	parser_for : _
	is_eq : _
}

# A nominal whose parser the compiler derives can be reached BOTH on its own
# and as a nested shape inside another derived codec. Both reads produce a
# generated-parser contract for the same shape, encoding, state and error row,
# so both must record the same callable types with JSON's format-owned error.
expect {
	result : Try(Req, [InvalidJson(Str)])
	result = Json.parse("{\"name\":\"a\"}")
	result == Ok(Req.{ name: "a" })
}

expect {
	result : Try([Wrap(Req), Nothing], [InvalidJson(Str)])
	result = Json.parse("{\"Wrap\":{\"name\":\"a\"}}")
	result == Ok(Wrap(Req.{ name: "a" }))
}

# These formerly checked error-row widening. Errors now belong uniformly to
# the format: missing required fields use the same informative InvalidJson
# error both standalone and nested, without adding a compiler-owned tag.
expect {
	result : Try(Req, [InvalidJson(Str)])
	result = Json.parse("{}")
	result == Err(InvalidJson("Missing field: name"))
}

expect {
	result : Try([Wrap(Req), Nothing], [InvalidJson(Str)])
	result = Json.parse("{\"Wrap\":{}}")
	result == Err(InvalidJson("Missing field: name"))
}

# A different enclosing tag shape keeps its own generated parser while sharing
# the format's error type.
expect {
	result : Try([Only(Req)], [InvalidJson(Str)])
	result = Json.parse("{\"Only\":{\"name\":\"a\"}}")
	result == Ok(Only(Req.{ name: "a" }))
}
