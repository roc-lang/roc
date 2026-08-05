JsonOptionalFieldKinds :: [].{}

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
