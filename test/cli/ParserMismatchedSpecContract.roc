ParserMismatchedSpecContract :: [].{}

Format := {}.{
	start : Str, U64 -> Try(Str, [Other(Str), Third])
	start = |state, _| Ok(state)
	next : Str, U64, U64 -> Try(Str, [Other(Str), Third])
	next = |state, _, _| Ok(state)

	# Translating AFTER spec.parse does not make its callback contract valid.
	parse_tag_union = |encoding, spec, state| {
		result = Encoding.ParseTagUnionSpec.parse(spec, {
			tag: "Item",
			encoding,
			state,
			start_payloads: Format.start,
			next_payload: Format.next,
			finish_payloads: Format.start,
			missing: Other("unknown"),
		})
		match result {
			Ok(value) => Ok(value)
			Err(Other(message)) => Err(Syntax(message))
			Err(Third) => Err(Syntax("third"))
		}
	}
}

Payload := {}.{
	parser_for : Format -> (Str -> Try({ value : Payload, rest : Str }, [Syntax(Str)]))
	parser_for = |_| |_| Err(Syntax("child"))
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("") {
		Err(Syntax("child")) => True
		_ => False
	}
}
