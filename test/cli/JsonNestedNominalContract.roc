JsonNestedNominalContract :: [].{}

Req := { name : Str }.{
	parser_for : _
	is_eq : _
}

# A nominal whose parser the compiler derives can be reached BOTH on its own
# and as a nested shape inside another derived codec. Both reads produce a
# generated-parser contract for the same shape, encoding, state and error row,
# so both must record the same callable types: the nested read is validated at
# the enclosing error row rather than at a private child row.
expect {
	result : Try(Req, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"name\":\"a\"}")
	result == Ok(Req.{ name: "a" })
}

expect {
	result : Try([Wrap(Req), Nothing], [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"Wrap\":{\"name\":\"a\"}}")
	result == Ok(Wrap(Req.{ name: "a" }))
}

# The row an annotation names, not the minimal row the shape demands, is the
# one both reads share—an annotated row wider than the shape's own demands
# still pairs a standalone read with a nested one.
expect {
	result : Try(Req, [InvalidJson(Str), MissingRequiredField(Str), Unreachable])
	result = Json.parse("{\"name\":\"a\"}")
	result == Ok(Req.{ name: "a" })
}

expect {
	result : Try([Wrap(Req), Nothing], [InvalidJson(Str), MissingRequiredField(Str), Unreachable])
	result = Json.parse("{\"Wrap\":{\"name\":\"a\"}}")
	result == Ok(Wrap(Req.{ name: "a" }))
}

# Nesting the same nominal under a DIFFERENT enclosing row is a different
# contract, and keeps its own generated parser.
expect {
	result : Try([Only(Req)], [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"Only\":{\"name\":\"a\"}}")
	result == Ok(Only(Req.{ name: "a" }))
}
