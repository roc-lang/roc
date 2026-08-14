JsonOptionalFieldKinds :: [].{}

Counted := { name : Str, count : U8 ?? 10 }.{
	parser_for : _
	encoder_for : _
	is_eq : _
}
CountOnly := { count : U8 ?? 10 }.{
	parser_for : _
	is_eq : _
}
SelfFill := { count : U8 ?? 10, opt ?: Str }.{
	parser_for : _
	is_eq : _
}
Twenty := { count : U8 ?? 20 }.{
	parser_for : _
	is_eq : _
}

# An absent `?:` field parses to the missing state: `.?` reads it as
# Err(MissingField), so the `??` default fires.
expect {
	result : Try({ name : Str, count ?: U8 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"name\":\"a\"}")
	result == Ok({ name: "a" })
}

# A present `?:` field parses at its payload type into the present state.
expect {
	result : Try({ name : Str, count ?: U8 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"name\":\"a\",\"count\":7}")
	result == Ok({ name: "a", count: 7 })
}

# The parsed present slot reads back through `.?`.
expect {
	result : Try({ name : Str, count ?: U8 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"count\":7,\"name\":\"a\"}")
	count = match result {
		Ok(rec) => rec.?count ?? 99
		Err(_) => 0
	}
	count == 7
}

# Encoding a missing `?:` slot omits the field entirely.
expect {
	value : { name : Str, count ?: U8 }
	value = { name: "a" }

	Json.to_str(value) == "{\"name\":\"a\"}"
}

# Encoding a present `?:` slot emits the field with its payload encoder.
expect {
	value : { name : Str, count ?: U8 }
	value = { name: "a", count: 7 }

	Json.to_str(value) == "{\"count\":7,\"name\":\"a\"}"
}

# Round trip: encode of a parsed value reproduces the original text, and
# the optional field survives nesting.
expect {
	parsed : Try({ inner : { flag ?: U8 }, name : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	parsed = Json.parse("{\"inner\":{},\"name\":\"n\"}")
	match parsed {
		Ok(rec) => Json.to_str(rec) == "{\"inner\":{},\"name\":\"n\"}"
		Err(_) => "" == "x"
	}
}

# An absent `??` key materializes the archived default—the same
# construction-site omission semantics `{}` literals get.
expect {
	result : Try(Counted, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"name\":\"a\"}")
	result == Ok(Counted.{ name: "a" })
}

# A present `??` key parses at the field's inline type and wins over the
# default.
expect {
	result : Try(Counted, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"name\":\"a\",\"count\":7}")
	result == Ok(Counted.{ name: "a", count: 7 })
}

# An explicit null is NOT absence: it stays a parse error for a defaulted
# field, exactly as for required and `?:` fields.
expect {
	result : Try(CountOnly, [InvalidJson(Str)])
	result = Json.parse("{\"count\":null}")
	result == Err(Json.invalid_json)
}

# A record whose fields can all self-fill parses with a closed error row:
# no MissingRequiredField is demanded.
expect {
	result : Try(SelfFill, [InvalidJson(Str)])
	result = Json.parse("{}")
	result == Ok(SelfFill.{})
}

# Encode always emits a defaulted field, including the default itself.
expect {
	value : Counted
	value = Counted.{ name: "a" }

	Json.to_str(value) == "{\"count\":10,\"name\":\"a\"}"
}

# An all-self-filling record INSIDE A TAG-UNION PAYLOAD parses with a
# closed error row: the lowering mirrors of the checker's self-fill
# analysis must not demand MissingRequiredField (or route the payload to
# the invalid-value arm) when every payload field self-fills.
expect {
	result : Try([Wrap(SelfFill), Nothing], [InvalidJson(Str)])
	result = Json.parse("{\"Wrap\":{}}")
	result == Ok(Wrap(SelfFill.{}))
}

# Present keys inside the tag payload parse at their inline/payload types.
expect {
	result : Try([Wrap(SelfFill), Nothing], [InvalidJson(Str)])
	result = Json.parse("{\"Wrap\":{\"count\":7,\"opt\":\"x\"}}")
	result == Ok(Wrap(SelfFill.{ count: 7, opt: "x" }))
}

# A required row with the same SHAPE as a `??` row above is a DIFFERENT
# monotype (`Type.FieldDefault` rides on the row): the required twin's
# parser must still error MissingRequiredField, never fill the other
# row's default.
expect {
	result : Try({ count : U8 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{}")
	result == Err(MissingRequiredField("count"))
}

# Same shape, different default: distinct monotypes, and each derived
# parser fills its own row's default.
expect {
	result : Try(Twenty, [InvalidJson(Str)])
	result = Json.parse("{}")
	result == Ok(Twenty.{})
}
