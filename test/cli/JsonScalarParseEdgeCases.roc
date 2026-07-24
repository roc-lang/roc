JsonScalarParseEdgeCases :: [].{}

# RFC 8259 conformance edge cases for scalar (number, boolean, null) parsing:
# literal spellings, the number grammar's reject paths, and scalar tokenization
# at each delimiter.

bool_rejects : Str -> Bool
bool_rejects = |json| {
	result : Try(Bool, [InvalidJson(Str)])
	result = Json.parse(json)
	result == Err(Json.invalid_json)
}

# Parse with `skip` as an unknown (skipped) field, so a rejection is
# attributable to the skipped scalar's spelling rather than the target type.
skipped_scalar_rejects : Str -> Bool
skipped_scalar_rejects = |scalar| {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse(Str.concat(Str.concat("{\"skip\":", scalar), ",\"a\":7}"))
	result == Err(Json.invalid_json)
}

skipped_scalar_accepts : Str -> Bool
skipped_scalar_accepts = |scalar| {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse(Str.concat(Str.concat("{\"skip\":", scalar), ",\"a\":7}"))
	result == Ok({ a: 7 })
}

u64_parses_as : Str, U64 -> Bool
u64_parses_as = |json, expected| {
	result : Try(U64, [InvalidJson(Str)])
	result = Json.parse(json)
	result == Ok(expected)
}

u64_rejects : Str -> Bool
u64_rejects = |json| {
	result : Try(U64, [InvalidJson(Str)])
	result = Json.parse(json)
	result == Err(Json.invalid_json)
}

i64_parses_as : Str, I64 -> Bool
i64_parses_as = |json, expected| {
	result : Try(I64, [InvalidJson(Str)])
	result = Json.parse(json)
	result == Ok(expected)
}

f64_parses_as : Str, F64 -> Bool
f64_parses_as = |json, expected| {
	result : Try(F64, [InvalidJson(Str)])
	result = Json.parse(json)
	F64.is_float_eq(result.ok_or(1234.5), expected)
}

f64_rejects : Str -> Bool
f64_rejects = |json| {
	result : Try(F64, [InvalidJson(Str)])
	result = Json.parse(json)
	result == Err(Json.invalid_json)
}

dec_parses_as : Str, Dec -> Bool
dec_parses_as = |json, expected| {
	result : Try(Dec, [InvalidJson(Str)])
	result = Json.parse(json)
	result == Ok(expected)
}

# --- literal spellings must be exact ---

# wrong-case literals are rejected
expect bool_rejects("True")
expect bool_rejects("FALSE")
expect skipped_scalar_accepts("null")
expect skipped_scalar_rejects("Null")

# truncated literals are rejected, including at end of input
# spellchecker:ignore-next-line
expect bool_rejects("tru")
# spellchecker:ignore-next-line
expect bool_rejects("fals")
expect skipped_scalar_rejects("nul")

# literals with trailing junk are rejected
expect bool_rejects("truex")
expect skipped_scalar_rejects("null0")

# --- number grammar reject paths ---

# bare or dangling decimal point
expect f64_rejects(".5")
expect f64_rejects("1.")
expect f64_rejects("-.5")

# incomplete exponent
expect f64_rejects("1e")
expect f64_rejects("1e+")
expect f64_rejects("1e-")

# lone minus, leading plus
expect f64_rejects("-")
expect f64_rejects("+1")

# leading zeros
expect u64_rejects("00")
expect f64_rejects("-01")

# Infinity and hex are not JSON numbers
expect f64_rejects("Infinity")
expect u64_rejects("0x1F")

# trailing junk after a valid number
expect u64_rejects("42abc")
expect f64_rejects("1.2.3")
expect u64_rejects("12-3")

# --- number grammar accept paths ---

# uppercase exponent marker, explicit plus, negative exponent, zero exponent
expect f64_parses_as("1E5", 100000.0)
expect f64_parses_as("1e+5", 100000.0)
expect f64_parses_as("1e-5", 0.00001)
expect f64_parses_as("0e0", 0.0)

# sign, fraction, and exponent combined
expect f64_parses_as("-1.5e-3", -0.0015)
expect dec_parses_as("-1.5e-3", -0.0015)

# negative zero is a valid JSON number (signed and float targets accept it;
# unsigned targets reject any minus sign)
expect i64_parses_as("-0", 0)
expect f64_parses_as("-0", 0.0)
expect u64_rejects("-0")

# --- scalar tokenization at each delimiter ---

# a scalar terminated by each JSON whitespace byte
expect u64_parses_as("42 ", 42)
expect u64_parses_as("42\n", 42)
expect u64_parses_as("42\t", 42)
expect u64_parses_as("42\r", 42)

# scalars inside containers end at comma, closing brace, and closing bracket
expect {
	result : Try({ a : U64, b : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"a\":1,\"b\":2}")
	result == Ok({ a: 1, b: 2 })
}
expect {
	result : Try(List(U64), [InvalidJson(Str)])
	result = Json.parse("[1,2]")
	result == Ok([1, 2])
}

# an empty scalar (delimiter immediately after the colon or bracket) is rejected
expect {
	result : Try({ a : U64, b : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"a\":,\"b\":1}")
	result == Err(Json.invalid_json)
}
expect {
	result : Try(List(U64), [InvalidJson(Str)])
	result = Json.parse("[,1]")
	result == Err(Json.invalid_json)
}

# --- inter-token whitespace is exactly RFC 8259's ws ---

# all four RFC whitespace bytes are skippable around tokens
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse(" \t\n\r{ \"a\" :\t42 }\r\n")
	result == Ok({ a: 42 })
}

# a form feed (0x0C) between tokens is not JSON whitespace
expect {
	document = Str.from_utf8([91, 49, 44, 12, 50, 93])

	result : Try(List(U64), [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# a no-break space (U+00A0) before a value is not JSON whitespace
expect {
	document = Str.from_utf8([194, 160, 116, 114, 117, 101])

	result : Try(Bool, [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# a vertical tab (0x0B) after the document is not JSON whitespace
expect {
	document = Str.from_utf8([52, 50, 11])

	result : Try(U64, [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# a no-break space at each remaining grammar position rejects the document

# after a key, before the colon
expect {
	document = Str.from_utf8([123, 34, 97, 34, 194, 160, 58, 49, 125])

	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# after the colon, before the value
expect {
	document = Str.from_utf8([123, 34, 97, 34, 58, 194, 160, 49, 125])

	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# between object fields, after the comma
expect {
	document = Str.from_utf8([123, 34, 97, 34, 58, 49, 44, 194, 160, 34, 98, 34, 58, 50, 125])

	result : Try({ a : U64, b : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# before the object closer
expect {
	document = Str.from_utf8([123, 34, 97, 34, 58, 49, 194, 160, 125])

	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# after the array opener
expect {
	document = Str.from_utf8([91, 194, 160, 49, 93])

	result : Try(List(U64), [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# inside a skipped object, before a key
expect {
	document = Str.from_utf8([123, 34, 115, 107, 105, 112, 34, 58, 123, 194, 160, 34, 120, 34, 58, 49, 125, 44, 34, 97, 34, 58, 55, 125])

	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}
