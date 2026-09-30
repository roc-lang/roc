CustomNominalContainerCodecs :: [].{}

# These codecs deliberately use strings instead of their backing's protocol.
Words := List(Str).{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: Words.([parsed.value]), rest: parsed.rest })
	}
	encoder_for = |encoding| |Words.(words), state| Json.encode_str(encoding, Str.join_with(words, ""), state)
}

Pair := (Str, U8).{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: Pair.((parsed.value, 7)), rest: parsed.rest })
	}
	encoder_for = |encoding| |Pair.((text, _)), state| Json.encode_str(encoding, text, state)
}

Wrapped := Box(Str).{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: Wrapped.(Box.box(parsed.value)), rest: parsed.rest })
	}
	encoder_for = |encoding| |Wrapped.(boxed), state| Json.encode_str(encoding, Box.unbox(boxed), state)
}

# Record fields exercise all three nominal backing shapes.
expect {
	parsed : Try({ a : Words, b : Pair, c : Wrapped }, _)
	parsed = Json.parse("{\"a\":\"words\",\"b\":\"pair\",\"c\":\"box\"}")
	match parsed {
		Ok({ a: Words.(words), b: Pair.((text, number)), c: Wrapped.(boxed) }) =>
			words == ["words"] and text == "pair" and number == 7 and Box.unbox(boxed) == "box"
		Err(_) => False
	}
}

expect Json.to_str({ a: Words.(["wo", "rds"]), b: Pair.(("pair", 7)), c: Wrapped.(Box.box("box")) }) == "{\"a\":\"words\",\"b\":\"pair\",\"c\":\"box\"}"

# Tuple elements must retain nominal dispatch too.
expect {
	parsed : Try((Words, Pair, Wrapped), _)
	parsed = Json.parse("[\"words\",\"pair\",\"box\"]")
	match parsed {
		Ok((Words.(words), Pair.((text, number)), Wrapped.(boxed))) =>
			words == ["words"] and text == "pair" and number == 7 and Box.unbox(boxed) == "box"
		Err(_) => False
	}
}

expect Json.to_str((Words.(["wo", "rds"]), Pair.(("pair", 7)), Wrapped.(Box.box("box")))) == "[\"words\",\"pair\",\"box\"]"

# Repeated list elements share the prepared custom codec.
expect {
	parsed : Try(List(Words), _)
	parsed = Json.parse("[\"one\",\"two\"]")
	match parsed {
		Ok([Words.(one), Words.(two)]) => one == ["one"] and two == ["two"]
		_ => False
	}
}

expect Json.to_str([Words.(["one"]), Words.(["two"])]) == "[\"one\",\"two\"]"

expect {
	parsed : Try(List(Pair), _)
	parsed = Json.parse("[\"one\",\"two\"]")
	match parsed {
		Ok([Pair.((one, first)), Pair.((two, second))]) => one == "one" and two == "two" and first == 7 and second == 7
		_ => False
	}
}

expect Json.to_str([Pair.(("one", 7)), Pair.(("two", 7))]) == "[\"one\",\"two\"]"

expect {
	parsed : Try(List(Wrapped), _)
	parsed = Json.parse("[\"one\",\"two\"]")
	match parsed {
		Ok([Wrapped.(one), Wrapped.(two)]) => Box.unbox(one) == "one" and Box.unbox(two) == "two"
		_ => False
	}
}

expect Json.to_str([Wrapped.(Box.box("one")), Wrapped.(Box.box("two"))]) == "[\"one\",\"two\"]"
