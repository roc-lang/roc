JsonStringEscapes :: [].{}

# --- valid single-character escapes ---

expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\\"b\"}")

	result == Ok({ s: "a\"b" })
}

expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\\\b\"}")

	result == Ok({ s: "a\\b" })
}

expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\/b\"}")

	result == Ok({ s: "a/b" })
}

expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\b\\f\\n\\r\\tb\"}")

	result == Ok({ s: "a\u(8)\u(C)\n\r\tb" })
}

# --- unicode escapes ---

expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"caf\\u00e9\"}")

	result == Ok({ s: "café" })
}

# uppercase hex digits are accepted
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"caf\\u00E9 \\u4E2D\"}")

	result == Ok({ s: "café 中" })
}

# a surrogate pair combines into one code point (U+1F600)
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"\\uD83D\\uDE00\"}")

	result == Ok({ s: "😀" })
}

# back-to-back surrogate pairs each combine independently
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uD83D\\uDC4D\\uD83D\\uDC4E\\uD83C\\uDF89\"")

	result == Ok("👍👎🎉")
}

# \u0000 decodes to an embedded NUL
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"nul \\u0000 byte\"")

	result == Ok("nul \u(0) byte")
}

# the same string decodes identically from an all-ASCII document (every
# non-ASCII character escaped) and a raw-UTF-8 document (only mandatory escapes)
expect {
	ascii_doc : Try(Str, [InvalidJson(Str)])
	ascii_doc = Json.parse("\"caf\\u00e9 \\u4e2d \\ud83d\\ude00\\n\"")

	raw_doc : Try(Str, [InvalidJson(Str)])
	raw_doc = Json.parse("\"café 中 😀\\n\"")

	ascii_doc == Ok("café 中 😀\n") and raw_doc == Ok("café 中 😀\n")
}

# --- invalid escapes are rejected ---

# a valid escape does not permit a later raw control byte in the same string
expect {
	document = Str.from_utf8([34, 92, 116, 1, 34])

	result : Try(Str, [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# the control-character boundaries: raw 0x00 and raw 0x1F are both rejected
expect {
	document = Str.from_utf8([34, 0, 34])

	result : Try(Str, [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

expect {
	document = Str.from_utf8([34, 31, 34])

	result : Try(Str, [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# raw DEL (0x7F) is just past the control range and is valid unescaped
expect {
	document = Str.from_utf8([34, 127, 34])
	expected = Str.from_utf8([127])

	result : Try(Str, [InvalidJson(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	match (result, expected) {
		(Ok(parsed), Ok(want)) => parsed == want
		_ => False
	}
}

# unknown escape character
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\xb\"}")

	result == Err(Json.invalid_json)
}

# input ends on a backslash
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"a\\")

	result == Err(Json.invalid_json)
}

# incomplete \u escape
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\u12\"}")

	result == Err(Json.invalid_json)
}

# non-hex digits in \u escape
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\uZZZZ\"}")

	result == Err(Json.invalid_json)
}

# unpaired high surrogate
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\uD83Db\"}")

	result == Err(Json.invalid_json)
}

# unpaired low surrogate
expect {
	result : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"s\": \"a\\uDE00b\"}")

	result == Err(Json.invalid_json)
}

# --- escapes beyond top-level string values ---

# escaped object keys decode
expect {
	result : Try({ a_b : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"a\\u005fb\": \"ok\"}")

	result == Ok({ a_b: "ok" })
}

# escapes inside skipped unknown fields do not break known-field decoding
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"junk\": \"say \\\"hi\\\"\\n\", \"a\": 7}")

	result == Ok({ a: 7 })
}

# escaped strings inside lists
expect {
	result : Try(List(Str), [InvalidJson(Str)])
	result = Json.parse("[\"a\\tb\", \"c\"]")

	result == Ok(["a\tb", "c"])
}

# --- encode -> parse round-trip ---

expect {
	original = { s: "quote \" backslash \\ newline \n tab \t" }

	round_tripped : Try({ s : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	round_tripped = match Json.to_str_try(original) {
		Ok(encoded) => Json.parse(encoded)
		Err(_) => Err(Json.invalid_json)
	}

	round_tripped == Ok(original)
}

# escaped dict keys decode to their unescaped form
expect {
	result : Try(Dict(Str, U64), [InvalidJson(Str)])
	result = Json.parse("{\"a\\\"b\": 1, \"caf\\u00e9\": 2}")

	match result {
		Ok(scores) => scores.get("a\"b") == Ok(1) and scores.get("café") == Ok(2)
		Err(_) => False
	}
}

# escaped tag names decode to their unescaped form
expect {
	result : Try([Multi(Str, U64, Bool)], [InvalidJson(Str)])
	result = Json.parse("{\"\\u004Dulti\":[\"tag\",9,true]}")

	result == Ok(Multi("tag", 9, True))
}

# unterminated string: escaped quote followed by end of input
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"abc\\\"")

	result == Err(Json.invalid_json)
}

# a realistic API-response payload: escaped newlines in body text, quotes in
# a bio, an escaped-slash URL, a unicode name, and skipped unknown fields
# containing escapes
expect {
	result : Try(
		{
			author : { bio : Str, name : Str },
			body : Str,
			subtitle : Try(Str, [Missing]),
			url : Str,
		},
		[InvalidJson(Str), MissingRequiredField(Str)],
	)
	result = Json.parse("{\"body\": \"First line.\\nSecond line with \\\"emphasis\\\".\", \"author\": {\"name\": \"Ren\\u00e9e\", \"bio\": \"calls herself \\\"Ren\\\"\", \"avatar\": \"https:\\/\\/cdn.example.com\\/r.png\"}, \"url\": \"https:\\/\\/example.com\\/articles\\/1\", \"reactions\": [{\"emoji\": \"\\ud83d\\udc4d\", \"count\": 3}]}")

	result
		== Ok({
			author: { bio: "calls herself \"Ren\"", name: "Renée" },
			body: "First line.\nSecond line with \"emphasis\".",
			subtitle: Err(Missing),
			url: "https://example.com/articles/1",
		})
}

# --- scanner boundary cases ---

# empty string body: the closing quote is the first byte
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\"")

	result == Ok("")
}

# a body well past small-string size with escapes scattered throughout
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"line one\\nline two\\nline three\\nline four\\nline five\\nline six\\nline seven\\nline eight\\nsays \\\"kree\\\" caf\\u00e9\"")

	result == Ok("line one\nline two\nline three\nline four\nline five\nline six\nline seven\nline eight\nsays \"kree\" café")
}

# escaped quote as the very first character of the body
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\\"abc\"")

	result == Ok("\"abc")
}

# a run of consecutive escaped backslashes
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\\\\\\\\"")

	result == Ok("\\\\")
}

# escaped backslash immediately followed by an escaped quote
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\\\\\\"\"")

	result == Ok("\\\"")
}

# escaped backslash as the last character of the value
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"abc\\\\\"")

	result == Ok("abc\\")
}

# a clean string and escaped strings in one document take their own paths
expect {
	result : Try(List(Str), [InvalidJson(Str)])
	result = Json.parse("[\"clean\", \"esc\\n\", \"clean2\"]")

	result == Ok(["clean", "esc\n", "clean2"])
}

# --- skip-path cases (skipped strings are validated but never decoded) ---

# invalid escape inside a nested skipped subtree is still rejected
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":{\"deep\":[\"ok\",\"bad\\qx\"]},\"a\":7}")

	result == Err(Json.invalid_json)
}

# a raw control byte inside a skipped string is still rejected
expect {
	prefix = Str.to_utf8("{\"skip\":\"ab")
	suffix = Str.to_utf8("cd\",\"a\":7}")
	document = Str.from_utf8(List.concat(prefix, List.concat([1], suffix)))

	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = match document {
		Ok(value) => Json.parse(value)
		Err(_) => Err(Json.invalid_json)
	}

	result == Err(Json.invalid_json)
}

# unterminated skipped string
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"a\":7,\"skip\":\"abc")

	result == Err(Json.invalid_json)
}

# skipped string ending in a lone backslash at end of input
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"a\":7,\"skip\":\"abc\\")

	result == Err(Json.invalid_json)
}

# invalid escapes rejected in skipped positions, mirroring the value cases
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":\"a\\xb\",\"a\":7}")

	result == Err(Json.invalid_json)
}

expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":\"a\\u12z\",\"a\":7}")

	result == Err(Json.invalid_json)
}

expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":\"a\\uZZZZ\",\"a\":7}")

	result == Err(Json.invalid_json)
}

expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":\"a\\uD83Db\",\"a\":7}")

	result == Err(Json.invalid_json)
}

expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":\"a\\uDE00b\",\"a\":7}")

	result == Err(Json.invalid_json)
}

# escaped key on a skipped nested object (the skip_json_object key site)
expect {
	result : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	result = Json.parse("{\"skip\":{\"k\\\"ey\":\"v\"},\"a\":7}")

	result == Ok({ a: 7 })
}

# --- rare error branches ---

# high surrogate too close to end of input for a pair to fit
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uD83D")

	result == Err(Json.invalid_json)
}

# high surrogate followed by a \u escape that is not a low surrogate
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uD83D\\u0041\"")

	result == Err(Json.invalid_json)
}

# input truncated in the middle of \u hex digits
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\u12")

	result == Err(Json.invalid_json)
}

# --- UTF-8 encoding tier boundaries ---

# last 1-byte and first 2-byte code points
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\u007F\\u0080\"")

	result == Ok("\u(7F)\u(80)")
}

# last 2-byte and first 3-byte code points
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\u07FF\\u0800\"")

	result == Ok("\u(7FF)\u(800)")
}

# last 3-byte code point, and the first 4-byte one (also the minimal surrogate pair)
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uFFFF\\uD800\\uDC00\"")

	result == Ok("\u(FFFF)\u(10000)")
}

# --- escaped strings in tuple elements ---

expect {
	result : Try((Str, Str), [InvalidJson(Str)])
	result = Json.parse("[\"a\\\"b\", \"c\\\\d\"]")

	result == Ok(("a\"b", "c\\d"))
}

# --- encoder exact-output goldens ---

# the five named control escapes are emitted in short form
expect Json.to_str("\u(8)\u(9)\u(A)\u(C)\u(D)") == "\"\\b\\t\\n\\f\\r\""

# other control characters are emitted as lowercase \u00xx
expect Json.to_str("\u(0)\u(1)\u(1F)") == "\"\\u0000\\u0001\\u001f\""

# quotes and backslashes are escaped; solidus is not
expect Json.to_str("a\"b\\c/d") == "\"a\\\"b\\\\c/d\""

# non-ASCII text and DEL pass through as raw UTF-8
expect Json.to_str("café 中 \u(1F600)\u(7F)") == "\"café 中 \u(1F600)\u(7F)\""

# escaping applies inside containers: records (alphabetical field order),
# lists, tuples, and tag payloads
expect Json.to_str({ b: "q\"", a: "n\n" }) == "{\"a\":\"n\\n\",\"b\":\"q\\\"\"}"

expect Json.to_str(["a\tb", "c\\d"]) == "[\"a\\tb\",\"c\\\\d\"]"

expect Json.to_str(("x\n", "\"y\"")) == "[\"x\\n\",\"\\\"y\\\"\"]"

expect {
	value : [Wrap(Str)]
	value = Wrap("q\"")

	Json.to_str(value) == "{\"Wrap\":\"q\\\"\"}"
}

# --- encode -> parse round trips for string-heavy container shapes ---

expect {
	original = ["a\tb", "say \"hi\"", "c\\d"]

	round_tripped : Try(List(Str), [InvalidJson(Str)])
	round_tripped = Json.parse(Json.to_str(original))

	round_tripped == Ok(original)
}

expect {
	original = ("quote \"", "slash \\ tab \t")

	round_tripped : Try((Str, Str), [InvalidJson(Str)])
	round_tripped = Json.parse(Json.to_str(original))

	round_tripped == Ok(original)
}

expect {
	original : Dict(Str, Str)
	original = Dict.from_list([("k\"1", "line\none"), ("k\\2", "café 😀")])

	round_tripped : Try(Dict(Str, Str), [InvalidJson(Str)])
	round_tripped = Json.parse(Json.to_str(original))

	round_tripped == Ok(original)
}

expect {
	original : [Wrap(Str)]
	original = Wrap("nested \"quotes\" and \\backslashes\\\nnewline")

	round_tripped : Try([Wrap(Str)], [InvalidJson(Str)])
	round_tripped = Json.parse(Json.to_str(original))

	round_tripped == Ok(original)
}

# the maximum code point (U+10FFFF) arrives as the highest surrogate pair
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uDBFF\\uDFFF\"")

	expected = Str.from_utf8([244, 143, 191, 191])

	match (result, expected) {
		(Ok(parsed), Ok(want)) => parsed == want
		_ => False
	}
}

# a surrogate pair truncated inside its second escape is rejected
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uD83D\\uDE\"")

	result == Err(Json.invalid_json)
}

# a high surrogate followed by literal characters is rejected
expect {
	result : Try(Str, [InvalidJson(Str)])
	result = Json.parse("\"\\uD83Dxy\"")

	result == Err(Json.invalid_json)
}
